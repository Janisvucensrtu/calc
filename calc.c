// Janis Vucens 251RDC065
// https://github.com/Janisvucensrtu/calc
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: chatgpt, geeksforgeeks, stackoverflow, cp-algorithms, cppreference, w3schools

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define STUDENT_NAME "janis"
#define STUDENT_LASTNAME "vucens"
#define STUDENT_ID "251RDC065"

typedef enum { T_INT, T_PLUS, T_MINUS, T_STAR, T_SLASH, T_POW, T_LPAREN, T_RPAREN, T_EOL, T_BAD } TokKind;

typedef struct {
    TokKind kind;
    //0-based offset in the whole file; error = pos + 1
    size_t pos;
    //value of T_INT
    long long val;
} Token;

typedef struct {
    // whole file, NUL-terminated at buf[len]
    char *buf;
    size_t len;
    // next unread offset
    size_t pos;
    // the token the parser is looking at
    Token tok;
    // 0 = none, else the 1-based error position
    size_t err;
} Parser;

typedef struct { char *data; size_t len, cap; } Buffer;

static void *xrealloc(void *p, size_t n) {
    void *q = realloc(p, n ? n : 1);
    if (!q) {
        fprintf(stderr, "calc: out of memory\n");
        exit(1);
    }
    return q;
}
//capacity doubles when full, to minimize reallocations. https://www.geeksforgeeks.org/dsa/introduction-to-amortized-analysis/
static void buf_append(Buffer *b, const char *s, size_t n) {
    if (b->len + n > b->cap) {
        size_t nc = b->cap ? b->cap : 4096;
        while (nc < b->len + n) nc *= 2;
        b->data = xrealloc(b->data, nc);
        b->cap = nc;
    }
    memcpy(b->data + b->len, s, n);
    b->len += n;
}

//positions: p->pos is an offset into the one big buffer and is never reset between lines, so every '\n' and every comment character is counted for
//free: offset + 1 is exactly the 1-based position in the file.
//a bad character is returned as a T_BAD token instead of being an error here; the parser reports it only when it actually reaches it, which keeps "the first error" in parse order. with the help of ai
static void advance(Parser *p) {
    char *b = p->buf;
    size_t i = p->pos;
    Token t;

    while (i < p->len && (b[i] == ' ' || b[i] == '\t')) i++;
    t.pos = i;
    t.val = 0;
    //EOL sits ON the '\n' (or at len when the last line has no '\n'), so "missing operand" / "missing )" errors point at the newline, or at file length + 1 for an unterminated last line.
    if (i >= p->len || b[i] == '\n') {
        t.kind = T_EOL;
        //not consumed: the line loop skips the '\n'
        p->pos = i;
        p->tok = t;
        return;
    }
    if (isdigit((unsigned char)b[i])) {
        unsigned long long v = 0;
        //b[len] is NUL: stops
        while (isdigit((unsigned char)b[i])) {
            v = v * 10u + (unsigned long long)(b[i] - '0');
            i++;
        }
        t.kind = T_INT;
        t.val = (long long)v;
        p->pos = i;
        p->tok = t;
        return;
    }
    p->pos = i + 1;
    switch (b[i]) {
    case '+': t.kind = T_PLUS; break;
    case '-': t.kind = T_MINUS; break;
    case '/': t.kind = T_SLASH; break;
    case '(': t.kind = T_LPAREN; break;
    case ')': t.kind = T_RPAREN; break;
    case '*':
        if (b[i + 1] == '*') {t.kind = T_POW; p->pos = i + 2; }
        else t.kind = T_STAR;
        break;
    default: t.kind = T_BAD; break;
    }
    p->tok = t;
}
//tokenizer
//expr    := term  { ('+' | '-') term }
//term    := power { ('*' | '/') power }
//power   := primary [ '**' power ]
//primary := NUMBER | '(' expr ')'
//precedence: each level is built only from the next-tighter one, so in "2 + 3 * 4" the '*' is consumed inside term() before expr() sees it.
//+ - * / are loops, so they group to the left: 8 - 3 - 2 == (8 - 3) - 2.
//power() calls itself for the exponent, so 2 ** 3 ** 2 groups as 2 ** (3 ** 2).
//every function stops as soon as p->err is set; its return value is then meaningless. Integer + - * use unsigned arithmetic because signed overflow is undefined behaviour in C and unsigned wrap-around is not. with the help of ai
static long long parse_expr(Parser *p);

static long long parse_primary(Parser *p) {
    if (p->tok.kind == T_INT) {
        long long v = p->tok.val;
        advance(p);
        return v;
    }
    if (p->tok.kind == T_LPAREN) {
        advance(p);
        long long v = parse_expr(p);
        if (p->err) return 0;
        //missing ')' is reported at the token we are looking at: the '\n' (or end of file) for "(2 + 3", or whatever stray token is there.
        if (p->tok.kind != T_RPAREN) {
            p->err = p->tok.pos + 1;
            return 0;
        }
        advance(p);
        return v;
    }

    p->err = p->tok.pos + 1;
    return 0;
}

static long long int_pow(long long base, long long exp) {
    // repeated squaring: O(log exp) steps, so huge exponents stay fast https://cp-algorithms.com/algebra/binary-exp.html
    unsigned long long r = 1, b = (unsigned long long)base;
    while (exp > 0) {
        if (exp & 1) r *= b;
        b *= b;
        exp >>= 1;
    }
    return (long long)r;
}
//with the help of ai
static long long parse_power(Parser *p) {
    long long base = parse_primary(p);
    if (p->err || p->tok.kind != T_POW) return base;
    advance(p);
    //where the exponent starts
    size_t exp_pos = p->tok.pos;
    //recursion => right-associative
    long long e = parse_power(p);
    if (p->err) return 0;
    //The exponent is the offending operand, so the error is reported at its start. x ** 0 is 1 for every x, including 0 ** 0 (int_pow).
    if (e < 0) {
        p->err = exp_pos + 1;
        return 0;
    }
    return int_pow(base, e);
}
//with the help of ai
static long long parse_term(Parser *p) {
    long long l = parse_power(p);
    while (!p->err && (p->tok.kind == T_STAR || p->tok.kind == T_SLASH)) {
        TokKind op = p->tok.kind;
        advance(p);
        size_t rhs_pos = p->tok.pos;
        long long r = parse_power(p);
        if (p->err) return 0;
        if (op == T_STAR) {
            l = (long long)((unsigned long long)l * (unsigned long long)r);
        } else {
            //The divisor is what is wrong, so report at its start.
            if (r == 0) {
                p->err = rhs_pos + 1;
                return 0;
            }
            //avoids LLONG_MIN / -1 UB
            if (r == -1) l = (long long)(0ULL - (unsigned long long)l);
            //C truncates toward zero, as required
            else l = l / r;
        }
    }
    return l;
}

static long long parse_expr(Parser *p) {
    long long l = parse_term(p);
    while (!p->err && (p->tok.kind == T_PLUS || p->tok.kind == T_MINUS)) {
        TokKind op = p->tok.kind;
        advance(p);
        long long r = parse_term(p);
        if (p->err) return 0;
        if (op == T_PLUS) l = (long long)((unsigned long long)l + (unsigned long long)r);
        else l = (long long)((unsigned long long)l - (unsigned long long)r);
    }
    return l;
}
//returns 0 and fills out (one result per expression), or the 1-basedposition of the first error, stops at the first error: nothing later can change the output. with the help of ai
static size_t evaluate(char *buf, size_t len, Buffer *out) {
    Parser p;
    char tmp[32];
    size_t pos = 0;

    memset(&p, 0, sizeof p);
    p.buf = buf;
    p.len = len;

    while (pos < len) {
        size_t i = pos;
        while (i < len && (buf[i] == ' ' || buf[i] == '\t')) i++;
        if (i >= len) break;
        //blank line: no output
        if (buf[i] == '\n') { pos = i + 1; continue; }
        // comment line: skipped,
        if (buf[i] == '#') {
            // but still counted since
            while (i < len && buf[i] != '\n') i++;
            // positions are offsets
            pos = i < len ? i + 1 : i;
            continue;
        }

        p.pos = pos;
        advance(&p);
        long long v = parse_expr(&p);
        //after a full expression only the end of the line may follow; anything else (e.g. the extra ')' in "(1 + 2))") is an error there.
        if (!p.err && p.tok.kind != T_EOL) p.err = p.tok.pos + 1;
        if (p.err) return p.err;

        int n = snprintf(tmp, sizeof tmp, "%lld\n", v);
        buf_append(out, tmp, (size_t)n);
        pos = p.tok.pos;//offset of the '\n' (or len) ending the line
        if (pos < len) pos++;
    }
    return 0;
}
//reads the whole file in one go, the size is only a first guess for the capacity; the buffer still doubles if it was too small https://www.w3schools.com/c/c_files_read.php
static int read_file(const char *path, char **data, size_t *len) {
    FILE *f = fopen(path, "rb");
    if (!f) return -1;
    struct stat st;
    size_t cap = 65536;
    if (stat(path, &st) == 0 && st.st_size > 0) cap = (size_t)st.st_size + 1;
    char *buf = xrealloc(NULL, cap + 1);
    size_t n = 0, got;
    do {
        if (n == cap) {
            cap *= 2;
            buf = xrealloc(buf, cap + 1);
        }
        got = fread(buf + n, 1, cap - n, f);
        n += got;
    } while (got > 0);
    int bad = ferror(f);
    fclose(f);
    if (bad) { free(buf); return -1; }
    //lets the tokenizer look one character past the end
    buf[n] = '\0';
    *data = buf;
    *len = n;
    return 0;
}

static void usage(void) {
    fprintf(stderr, "usage: calc -o OUTDIR input.txt\n");
}

int main(int argc, char **argv) {
    const char *outdir = NULL, *input = NULL;

    for (int k = 1; k < argc; k++) {
        if (strcmp(argv[k], "-o") == 0 && k + 1 < argc) outdir = argv[++k];
        else if (argv[k][0] != '-' && !input) input = argv[k];
        else { fprintf(stderr, "calc: bad argument '%s'\n", argv[k]); usage(); return 2; }
    }
    if (!outdir || !input) { usage(); return 2; }

    char *data = NULL;
    size_t len = 0;
    if (read_file(input, &data, &len) != 0) {
        fprintf(stderr, "calc: cannot read '%s': %s\n", input, strerror(errno));
        return 1;
    }

    Buffer out = {NULL, 0, 0};
    size_t err = evaluate(data, len, &out);
    //an ERROR line is a normal result: exit code stays 0
    if (err) {
        char tmp[48];
        int n = snprintf(tmp, sizeof tmp, "ERROR:%zu\n", err);
        //no partial results
        out.len = 0;
        buf_append(&out, tmp, (size_t)n);
    }
    free(data);

    if (mkdir(outdir, 0777) != 0 && errno != EEXIST) {
        fprintf(stderr, "calc: cannot create '%s': %s\n", outdir, strerror(errno));
        free(out.data);
        return 1;
    }
    //<OUTDIR>/<input_base>_<name>_<lastname>_<id>.txt, input_base = file name without directories and without ".txt". https://en.cppreference.com/c/string/byte/ and /strrchr/
    const char *slash = strrchr(input, '/');
    const char *file = slash ? slash + 1 : input;
    size_t flen = strlen(file);
    if (flen > 4 && strcmp(file + flen - 4, ".txt") == 0) flen -= 4;
    size_t plen = strlen(outdir) + flen + 80;
    char *path = xrealloc(NULL, plen);
    snprintf(path, plen, "%s/%.*s_%s_%s_%s.txt", outdir, (int)flen, file,
             STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);

    int rc = 0;
    FILE *f = fopen(path, "wb");
    if (!f || fwrite(out.data, 1, out.len, f) != out.len) {
        fprintf(stderr, "calc: cannot write '%s': %s\n", path, strerror(errno));
        rc = 1;
    }
    if (f && fclose(f) != 0) rc = 1;
    free(path);
    free(out.data);
    return rc;
}