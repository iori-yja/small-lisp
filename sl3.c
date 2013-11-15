/* A minimal Lisp interpreter
   Copyright 2004 Andru Luvisi

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License , or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#define error(X) do { fprintf(stderr, "%s\n", X); exit(1); } while (0)

int line_num = 1;
int total_malloc = 0;

typedef struct {
	int num;
	unsigned int denomi;
} rational_t;

/*** List Structured Memory ***/
enum otype { NIL, TEE, COFFEE, INT, REAL, RATIONAL, STRLITERAL, SYM,
	CONS, PROC, PRIMOP };
typedef struct obj {
	enum otype type;
	int line_num;
	union {
		int integer;
		double real;
		rational_t rational;
		char *strliteral;
		char *symbol;
		struct obj *primop;
		struct obj *proc[3];
		struct obj *p[2];
	} object;
} obj;

typedef struct {
		int (* intmath)();
		double (* realmath)();
		rational_t (* ratmath)();
} manipulations_t;

typedef obj * (*primop)(obj *);
obj *all_symbols, *top_env, *nil, *tee, *coffee, *quote, 
    *s_if, *s_lambda, *s_define, *s_setb, *s_begin, *s_let;

unsigned int bgcd(int, int);

#define cons(X, Y)            omake(CONS, 2, (X), (Y))

obj *car(obj *X) {
  if (X == 0) {
    fprintf(stderr, "warning: car argument null on line %d\n", line_num);
    return nil;
  }
  if (X == nil)
    return nil;
  if (X->type != CONS) {
    fprintf(stderr, "warning: car argument not a list (%d) on line %d\n", (int) X->object.integer, (int) X->line_num);
    return nil;
  }
  return X->object.p[0];
}

obj *cdr(obj *X) {
	if (X == nil)
		return nil;
	if (X->type != CONS) {
		fprintf(stderr, "warning: cdr argument not a list on line %d\n", X->line_num); 
		return nil;    
	}
	if (X->object.p[1] == 0) {
		fprintf(stderr, "error: cdr list element is zero-pointer at %d\n", X->line_num);
		return nil;
	}
	if (X->type == PROC)
		return X->object.proc[1];
	else
		return X->object.p[1];
}


#define caar(p)         car(car(p))
#define cadr(p)         car(cdr(p))
#define cdar(p)         cdr(car(p))
#define cddr(p)         cdr(cdr(p))

#define caaar(p)        car(caar(p)) 
#define cdaar(p)        cdr(caar(p)) 
#define caadr(p)        car(cadr(p))
#define cdadr(p)        cdr(cadr(p))
#define cadar(p)        car(cdar(p))
#define cddar(p)        cdr(cdar(p))
#define caddr(p)        car(cddr(p))
#define cdddr(p)        cdr(cddr(p))

#define caaaar(p)        car(caaar(p)) 
#define cddddr(p)        cdr(cdddr(p))
#define setcar(X,Y)           (((X)->object.p[0]) = (Y))
#define setcdr(X,Y)           (((X)->object.p[1]) = (Y))

#define mknil()               omake(NIL, 0)
#define isnil(X)              ((X)->type == NIL)

#define mktee()               omake(TEE, 0)
#define istee(X)              ((X)->type == TEE)

#define mkcoffee()            omake(COFFEE, 0)
#define iscoffee(X)           ((X)->type == COFFEE)

#define mkint(X)              omake(INT, 1, (X))
#define intval(X)             ((int)((X)->type == INT ? (X)->object.integer : 0)) // intval for INT only
#define isint(X)              ((X)->type == INT)

#define mkreal(X)             omake(REAL, 1, (X))
#define realval(X)            ((double)((X)->type == REAL ? (X)->object.real : 0))
#define isreal(X)             ((X)->type == REAL)

#define mkrat(NUM, DENOMI)   omake(RATIONAL, 2, (NUM), (DENOMI))
#define ratval(X)            ((rational_t)((X)->object.rational))
#define israt(X)             ((X)->type == RATIONAL)

#define mkstrlit(X)          omake(STRLITERAL, (X))
#define strlitval(X)         ((char *)((X)->type == STRLITERAL ? (X)->object.strliteral : 0))
#define isstrlit(X)         ((X)->type == STRLITERAL)

#define mksym(X)              omake(SYM, 1, (X))
#define symname(X)            ((char *)((X)->object.symbol))

#define mkprimop(X)           omake(PRIMOP, 1, (X))
#define primopval(X)          ((primop)(X)->object.primop)

#define mkproc(X,S,ENV)       omake(PROC, 3, (X), (S), (ENV))
#define procargs(X)           ((X)->object.proc[0])
#define proccode(X)           ((X)->object.proc[1])
#define procenv(X)            ((X)->object.proc[2])

#define ispair(X)             (((X)->object.p[1]) != (nil))

obj *omake(enum otype type, int count, ...) {
  obj *newobj;
  va_list ap;
  int i;
  va_start(ap, count);
  int object_size = sizeof(obj) + (count - 1)*sizeof(obj *);
  total_malloc += object_size;

  newobj = (obj *) malloc(object_size);
  newobj->type = type;
  newobj->line_num = line_num;
  for (i = 0; i < count; i++) {
		switch (type) {
			case NIL:
			case TEE:
			case COFFEE:
				fprintf(stderr, "nil, tee, coffee won't take any args\n");
				break;
			case INT:
				newobj->object.integer = va_arg(ap, int);
				break;
			case REAL:
				newobj->object.real = va_arg(ap, double);
				break;
			case RATIONAL:
				if (!i) // rational is (num / denomi), so args is mkrat(num, denomi)
					newobj->object.rational.num = va_arg(ap, int);
				else
					newobj->object.rational.denomi = va_arg(ap, unsigned int);
				break;
			case STRLITERAL:
				newobj->object.strliteral = va_arg(ap, char *);
				break;
			case SYM:
				newobj->object.symbol = va_arg(ap, char *);
				break;
			case PRIMOP:
				newobj->object.primop = va_arg(ap, obj *);
				break;
			case PROC:
				newobj->object.proc[i] = va_arg(ap, obj *);
				break;
			case CONS:
				newobj->object.p[i] = va_arg(ap, obj *);
				break;
			default:
				break;
		}
	}
  va_end(ap);
  return newobj;
}

obj *findsym(char *keyword) {
  obj *symlist;
	char *sym;

	if (!strcmp(keyword, "nil"))
		return nil;
	if (!strcmp(keyword, "tee"))
		return tee;
	if (!strcmp(keyword, "coffee"))
		return coffee;

  for (symlist = all_symbols; !isnil(symlist); symlist = cdr(symlist)) {
    if (NULL != (sym = symname(car(symlist))) && !isnil(car(symlist))) {
			if (!strcmp(keyword, sym))
				return symlist;
		}
	}
  return nil;
}

obj *intern(char *newsymbol) {
  obj *op = findsym(newsymbol);
  if (!isnil(op)) return car(op);
  op = mksym(newsymbol);
  all_symbols = cons(op, all_symbols);
  return op;
}

/*** Environment ***/
#define extend(ENV, SYM, VAL) (cons(cons((SYM), (VAL)), (ENV)))

obj *multiple_extend(obj *env, obj *syms, obj *vals) {
  return isnil(syms) ? 
         env : 
         multiple_extend(extend(env, car(syms), car(vals)),
                         cdr(syms), cdr(vals));
}

obj *extend_top(obj *sym, obj *val) {
  setcdr(top_env, cons(cons(sym, val), cdr(top_env)));
  return val;
}

obj *assoc(obj *key, obj *alist) {
  if (isnil(alist)) return nil;
  if (car(car(alist)) == key) return car(alist);
  return assoc(key, cdr(alist));
}

/*** Input/Output ***/
FILE *ifp;
char *token_la;
int la_valid = 0;
#define MAXLEN 100
char buf[MAXLEN];
int bufused;

void add_to_buf(char ch) { if (bufused < MAXLEN - 1) buf[bufused++] = ch; }
char *buf2str()          { buf[bufused++] = '\0'; return strdup(buf); }
void setinput(FILE *fp)  { ifp = fp; }
void putback_token(char *token) { token_la = token; la_valid = 1; }

void myexit(int code);

char *gettoken() {
  int ch;
  char comment=0;

  bufused = 0;
  if (la_valid) {
		la_valid = 0;
		return token_la;
	}
  do {
    if ((ch = getc(ifp)) == EOF)
			myexit(0);

    if (ch == ';')
			comment = 1;
    if (ch == '\n') {
      comment = 0;
      line_num++;
    }

  } while(isspace(ch) || comment);


  add_to_buf(ch);
  if (strchr("()\'", ch)) return buf2str();
  for(;;) {
    if ((ch = getc(ifp)) == EOF) myexit(0);
    if (strchr("()\'", ch) || isspace(ch)) {
      ungetc(ch, ifp);
      return buf2str();
    }
    add_to_buf(ch);
  }
}

obj *readlist();

obj *readobj() {
  char *token;
	int vinculum; 

  token = gettoken();
  if (!strcmp(token, "(")) return readlist();
  if (!strcmp(token, "\'")) return cons(quote, cons(readobj(), nil));
	if (!strcmp(token, "#f")) return coffee;
	if (!strcmp(token, "#t")) return tee;

  if (token[(vinculum = strspn(token, "0123456789"))] == '\0'
     || (token[0] == '-' && strlen(token) > 1 &&
			 token[vinculum = strspn(token + 1, "0123456789") + 1] == '\0'))
    return mkint(atoi(token));

  else if ((token[strspn(token, "0123456789.")] == '\0'
				&& strlen(token) > 2)
			|| (token[0] == '-' && strlen(token) > 2 &&
				token[strspn(token + 1, "0123456789.") + 1] == '\0'))
    return mkreal(atof(token));

  else if ((token[strspn(token, "0123456789/")] == '\0'
				&& strlen(token) > 2)){
		token[vinculum] = '\0';
		return mkrat(atoi(token), atoi(token + vinculum + 1));
	}

	else if ((token[0] == '-' && strlen(token) > 3 &&
			 token[strspn(token + 1, "0123456789/") + 1] == '\0')) {
		token[vinculum] = '\0';
		return mkrat(atoi(token), atoi(token + vinculum + 1));
	}

  return intern(token);
}

obj *readlist() {
  char *token = gettoken();
  obj *tmp;
  if (!strcmp(token, ")")) return nil;
  if (!strcmp(token, ".")) {
    tmp = readobj();
    if (strcmp(gettoken(), ")")) exit(1);
    return tmp;
  }
  putback_token(token);
  tmp = readobj(); /* Must force evaluation order */
  return cons(tmp, readlist());
}

void writeobj(FILE *ofp, obj *op) {
	int num;
	unsigned int denomi;
	int gcd;

  switch(op->type) {
		case NIL:
			fprintf(ofp, "()");
			break;

		case TEE:
			fprintf(ofp, "#t");
			break;

		case COFFEE:
			fprintf(ofp, "#f");
			break;

    case INT:
			fprintf(ofp, "%d", intval(op));
			break;

		case REAL:
			fprintf(ofp, "%lf", realval(op));
			break;

		case RATIONAL:
			gcd = bgcd(ratval(op).num, (int) ratval(op).denomi);
			if (gcd == 0)
				abort();
			fprintf(ofp, "%d/%u", ratval(op).num/gcd, ratval(op).denomi/gcd);
			break;

    case CONS: 
		fprintf(ofp, "(");
		for(;;) {
			writeobj(ofp, car(op));
			if (isnil(cdr(op))) {
				fprintf(ofp, ")");
				break;
			}
			op = cdr(op);
			if (op->type != CONS) {
				fprintf(ofp, " . ");
				writeobj(ofp, op);
				fprintf(ofp, ")");
				break;
			}
			fprintf(ofp, " ");
		}
		break;
    case SYM:
		if (isnil(op)) fprintf(ofp, "()");
		else          fprintf(ofp, "%s", symname(op));
		break;
    case PRIMOP: fprintf(ofp, "#<PRIMOP>"); break;
    case PROC:   fprintf(ofp, "#<PROC>"); break;
    default: exit(1);
  }
}

/*** Evaluator (Eval/no Apply) ***/
obj *evlis(obj *exps, obj *env);

obj *eval(obj *exp, obj *env) {
obj *tmp, *proc, *vals;

  eval_start:

  if (exp == nil)
		return nil;

	switch (exp->type) {
		case NIL:
		case TEE:
		case COFFEE:
		case INT:
		case REAL:
		case RATIONAL:
			return exp;

		case SYM:
			tmp = assoc(exp, env);

			if (tmp == nil) {
        fprintf(stderr, "Unbound symbol ");
        writeobj(stderr, exp);
        fprintf(stderr, "\n");
        return nil;
      }
      return cdr(tmp);

    case CONS: 
      if (car(exp) == s_if) {
        if (eval(car(cdr(exp)), env) != coffee)
          return eval(car(cdr(cdr(exp))), env);
        else
          return eval(car(cdr(cdr(cdr(exp)))), env);
      }
      if (car(exp) == s_lambda)
        return mkproc(car(cdr(exp)), cdr(cdr(exp)), env);
      if (car(exp) == quote)
        return car(cdr(exp));
      if (car(exp) == s_define) {
				if (!isnil(cdr(cadr(exp)))) { //sugar syntax
					return (extend_top(caar(cdr(exp)),
                          eval(cons(s_lambda, cons(cdr(cadr(exp)), cddr(exp))),
														env)));
				}
				else {
					return(extend_top(cadr(exp),
                          eval(car(cdr(cdr(exp))), env)));
				}
			}
      if (car(exp) == s_setb) {
        obj *pair   = assoc(car(cdr(exp)), env);
        obj *newval = eval(car(cdr(cdr(exp))), env);
        setcdr(pair, newval);
        return newval;
      }
      if (car(exp) == s_begin) {
        exp = cdr(exp);
        if (exp == nil) return nil;
        for(;;) {
          if (cdr(exp) == nil) {
            exp = car(exp);
            goto eval_start;
          }
          eval(car(exp), env);
          exp = cdr(exp);
        }
      }
      proc = eval(car(exp), env);
      vals = evlis(cdr(exp), env);
      if (proc->type == PRIMOP)
        return (*primopval(proc))(vals);
      if (proc->type == PROC) {
        /* For dynamic scope, use env instead of procenv(proc) */
        env = multiple_extend(procenv(proc), procargs(proc), vals);
        exp = cons(s_begin, proccode(proc));
        goto eval_start;
      }
      printf("Bad PROC type\n");
      return nil;
    case PRIMOP: return exp;
    case PROC:   return exp;
  }
  /* Not reached */
  return exp; 
}

obj *evlis(obj *exps, obj *env) {
  if (exps == nil) return nil;
  return cons(eval(car(exps), env), 
              evlis(cdr(exps), env));
}

/*** Primitives ***/
obj *
numfold(
		manipulations_t *functs,
		obj *initial, obj *args)
{
	/* assume all symbol is int */
	int folded;
	double ffolded;
	rational_t rfolded;

	if (initial->type == INT)
		folded = initial->object.integer;
	else if (initial->type == REAL) {
		ffolded = initial->object.real;
		goto foldreal;
	}
	else if (initial->type == RATIONAL) {
		rfolded = initial->object.rational;
		goto foldrational;
	}

	for (; !isnil(args); args = cdr(args)) {
		if (car(args)->type == INT)
			folded = functs->intmath(folded, intval(car(args)));
		else if(car(args)->type == REAL) {
			/* fail to assume all symbol is int
			 * so we're going into floating point */
			ffolded = (double) folded;
			goto foldreal;
		}
		else if(car(args)->type == RATIONAL) {
			rfolded.num = folded;
			rfolded.denomi = 1;
			goto foldrational;
		}
		else abort();
	}
	return mkint(folded);

foldrational:
	for (; !isnil(args); args = cdr(args)) {
		if (car(args)->type == INT) {
			rational_t tmp = {intval(car(args)), 1};
			rfolded = functs->ratmath(rfolded, tmp);
		}
		else if (car(args)->type == RATIONAL) {
			rfolded = functs->ratmath(rfolded, ratval(car(args)));
		}
		else if (car(args)->type == REAL) {
			ffolded = (((double) rfolded.num) / ((double) rfolded.denomi));
			goto foldreal;
		}
		else
			abort();
	}
	return mkrat(rfolded.num, rfolded.denomi);

foldreal:
	for (; !isnil(args); args = cdr(args)) {
		if (car(args)->type == INT)
			ffolded = functs->realmath(ffolded, (double) intval(car(args)));
		else if (car(args)->type == REAL)
			ffolded = functs->realmath(ffolded, realval(car(args)));
		else if (car(args)->type == RATIONAL) {
			rational_t tmp = ratval(car(args));
			ffolded = functs->realmath(ffolded, ((double) tmp.num / (double) tmp.denomi));
		}
		else
			abort();
	}
	return mkreal(ffolded);
}

unsigned int
bgcd(int u, int v)
{
  int shift;
 
  /* GCD(0,v) == v; GCD(u,0) == u, GCD(0,0) == 0 */
  if (u == 0) return v;
  if (v == 0) return u;
  if (u < 0) u = - u;
  if (v < 0) v = -v;
 
  /* Let shift := lg K, where K is the greatest power of 2
        dividing both u and v. */
  for (shift = 0; ((u | v) & 1) == 0; ++shift) {
         u >>= 1;
         v >>= 1;
  }
 
  while ((u & 1) == 0)
    u >>= 1;
 
  /* From here on, u is always odd. */
  do {
       /* remove all factors of 2 in v -- they are not common */
       /*   note: v is not zero, so while will terminate */
       while ((v & 1) == 0)  /* Loop X */
           v >>= 1;
 
       /* Now u and v are both odd. Swap if necessary so u <= v,
          then set v = v - u (which is even). For bignums, the
          swapping is just pointer movement, and the subtraction
          can be done in-place. */
       if (u > v) {
         unsigned int t = v; v = u; u = t;}  // Swap u and v.
       v = v - u;                       // Here v >= u.
     } while (v != 0);
 
  /* restore common factors of 2 */
  return u << shift;
}

/* plus */
int
__intplus(int s, int t)
{
	return s + t;
}

double
__realplus(double s, double t)
{
	return s + t;
}

#define ismultiple(X, Y) (((X) > (2 * (Y))) ? ((X) > (Y)) ? (((X) % (Y)) == 0) : (((Y) % (X)) == 0) : ((X) == (2 * (Y))) ? 1 : 0)

rational_t
__ratplus(rational_t s, rational_t t)
{
	unsigned int ds, dt;
	rational_t ret;

	if ((ds = s.denomi) == (dt = t.denomi)) {
		ret.num = s.num + t.num;
		ret.denomi = ds;
	}
	else if ((ds > dt) && ismultiple(ds, dt)) {
		ret.num = s.num + (t.num * (int) (ds / dt));
		ret.denomi = ds;
	}
	else if ((ds < dt) && ismultiple(dt, ds)) {
		ret.num = (s.num * (int) (dt / ds)) + t.num;
		ret.denomi = dt;
	}
	else {
		ret.num = s.num * ((int) dt) + t.num * ((int) ds);
		ret.denomi = dt * ds;
		if (ret.num < (s.num * ((int) dt)) || ret.denomi < dt ) {
			int g = bgcd(ret.num, (int) ret.denomi);
			ret.num /= g;
			ret.denomi /= g;
		}
	}
	return ret;
}

obj *
prim_sum(obj *args)
{
	manipulations_t m = {
		__intplus,
		__realplus,
		__ratplus,
	};
	return numfold(&m, car(args), cdr(args));
}

/* subtract */
int
__intsub(int s, int t)
{
	return s - t;
}

double
__realsub(double s, double t)
{
	return s - t;
}

rational_t
__ratsub(rational_t s, rational_t t)
{
	int ds, dt;
	rational_t ret;

	if ((ds = s.denomi) != (dt = t.denomi)) {
		ret.num = s.num * dt - t.num * ds;
		ret.denomi = dt * ds;
		if (ret.num < (s.num * dt) || ret.denomi < dt ) {
			int g = bgcd(ret.num, ret.denomi);
			ret.num /= g;
			ret.denomi /= g;
		}
	}
	else if ((ds > dt) && ismultiple(ds, dt)) {
		ret.num = s.num - (t.num * ds / dt);
		ret.denomi = ds;
	}
	else if ((ds < dt) && ismultiple(dt, ds)) {
		ret.num = (s.num * dt / ds) - t.num;
		ret.denomi = dt;
	}
	else {
		ret.num = s.num - t.num;
		ret.denomi = ds;
	}
	return ret;
}

obj *
prim_sub(obj *args)
{
	manipulations_t m = {
		__intsub,
		__realsub,
		__ratsub,
	};
	return numfold(&m, car(args), cdr(args));
}

/* product */
int
__intprod(int s, int t)
{
	return s * t;
}

double
__realprod(double s, double t)
{
	return s * t;
}

rational_t
__ratprod(rational_t s, rational_t t)
{
	
}

obj *
prim_prod(obj *args)
{
	manipulations_t m = {
		__intprod,
		__realprod,
		__ratprod,
	};
	return numfold(&m, car(args), cdr(args));
}

/* division */
int
__intdiv(int s, int t)
{
	return s / t;
}

double
__realdiv(double s, double t)
{
	return s / t;
}

rational_t
__ratdiv(rational_t s, rational_t t)
{
	
}

obj *
prim_divide(obj *args)
{
	manipulations_t m = {
		__intdiv,
		__realdiv,
		__ratdiv,
	};
	return numfold(&m, car(args), cdr(args));
}

obj *prim_gt(obj *args) {
  return intval(car(args)) > intval(car(cdr(args))) ? tee : coffee;
}

obj *prim_lt(obj *args) {
  return intval(car(args)) < intval(car(cdr(args))) ? tee : coffee;
}
obj *prim_ge(obj *args) {
  return intval(car(args)) >= intval(car(cdr(args))) ? tee : coffee;
}
obj *prim_le(obj *args) {
  return intval(car(args)) <= intval(car(cdr(args))) ? tee : coffee;
}
obj *prim_numeq(obj *args) {
  return intval(car(args)) == intval(car(cdr(args))) ? tee : coffee;
}
obj *prim_symtable(void) {
	writeobj(stderr,all_symbols);
	return nil;
}

obj *prim_cons(obj *args) {
	return cons(car(args), car(cdr(args)));
}

obj *prim_car(obj *args)  {
	return car(car(args));
}

obj *prim_cdr(obj *args)  {
	return cdr(car(args));
}

obj *prim_nullcheck(obj *args) {
  return isnil(car(args)) ? tee : coffee;
}


/*** Helpers *****/

obj *prim_print(obj *args) {
  while(!isnil(args)) {
    writeobj(stdout, car(args));
    args = cdr(args);
    printf(" ");
  }
  printf("\n");
  return nil;
}

/*** Initialization ***/
void init_sl3() {
  nil = mknil();

  all_symbols = cons(nil, nil);
  top_env  = cons(cons(nil, nil), nil);
  tee      = mktee();
  coffee   = mkcoffee();
  extend_top(tee, tee);
  quote    = intern("quote");
  s_if     = intern("if");
  s_lambda = intern("lambda");
  s_define = intern("define");
  s_setb   = intern("set!");
  s_begin  = intern("begin");
  s_let    = intern("let");
  extend_top(intern("+"), mkprimop(prim_sum));
  extend_top(intern("-"), mkprimop(prim_sub));
  extend_top(intern("*"), mkprimop(prim_prod));
  extend_top(intern("/"), mkprimop(prim_divide));
  extend_top(intern("="), mkprimop(prim_numeq));

  extend_top(intern(">"), mkprimop(prim_gt));
  extend_top(intern(">="), mkprimop(prim_ge));

  extend_top(intern("<"), mkprimop(prim_lt));
  extend_top(intern("<="), mkprimop(prim_le));

  extend_top(intern("cons"), mkprimop(prim_cons));
  extend_top(intern("car"),  mkprimop(prim_car));
  extend_top(intern("cdr"),  mkprimop(prim_cdr));
  extend_top(intern("null?"),  mkprimop(prim_nullcheck));

  extend_top(intern("print"),  mkprimop(prim_print));
  extend_top(intern("symtable"), mkprimop(prim_symtable));
}

/*** Main Driver ***/
int main() {
  init_sl3();
  setinput(stdin);
  for(;;) {
    writeobj(stdout, eval(readobj(), top_env));
    printf("\n");
  }
	printf("bye");
  return 0;
}

void myexit(int code) {
  fprintf(stderr, "%d bytes left hanging\n", total_malloc);
  exit(code);
}

