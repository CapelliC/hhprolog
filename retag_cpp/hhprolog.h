/*
 * hhprolog: Hitchhiker Prolog
 * 
 * Version: 1.0.0
 * License: MIT
 * 
 * Copyright (c) 2018,2019 Carlo Capelli
 */

#ifndef HHPROLOG_H
#define HHPROLOG_H

#include <array>
#include <string>
#include <vector>
#include <cassert>
#include <stdexcept>
#include <unordered_map>

namespace hhprolog {

using namespace std;

typedef const string cstr;

typedef int32_t Int;
typedef uint32_t UInt;

template <typename T>
struct js_vect: vector<T> {
    js_vect(): vector<T>(){}
    js_vect(UInt s): vector<T>(s){}

    js_vect<T> slice(size_t s) const {
        js_vect<T> r;
        while (s < this->size()) {
            r.push_back(this->at(s++));
        }
        return r;
    }
    js_vect<T> concat(const js_vect<T> &s) const {
        js_vect<T> r(*this);
        for (auto v: s)
            r.push_back(v);
        return r;
    }
};
typedef js_vect<Int> IntS;
typedef js_vect<UInt> UIntS;

typedef IntS IntList;
typedef IntS IntStack;

typedef UIntS UIntList;
typedef UIntS UIntStack;

inline cstr operator+(cstr s, Int i) { return s + to_string(i); }
inline cstr operator+(cstr s, UInt i) { return s + to_string(i); }

typedef unordered_map<UInt, UIntS> t_imaps;
typedef unordered_map<UInt, UInt> t_IntMap;
typedef vector<t_IntMap> t_vmap;

struct Object {
    enum { t_null, t_int, t_string, t_vector } type;
    int _;

    Object() : type(t_null) {}
    explicit Object(Int i) : type(t_int), i(i) {}
    explicit Object(string s) : type(t_string), s(s) {}
    explicit Object(vector<Object> v) : type(t_vector), v(v) {}

    Int i;
    string s;
    vector<Object> v;

    string toString() const {
        switch(type) {
        case t_null:
            return "$null";
        case t_int:
            return to_string(i);
        case t_string:
            return s;
        case t_vector: {
            string j;
            for (auto a: v) {
                if (!j.empty())
                    j += ",";
                j += a.toString();
            }
            return "(" + j + ")";
        }}
        throw logic_error("invalid term");
    }
};

namespace Toks {
    typedef vector<string> Ts;
    typedef vector<Ts> Tss;
    typedef vector<Tss> Tsss;

    Tsss toSentences(string s);
    Tss maybeExpand(Ts Ws);
    Tss mapExpand(Tss Wss);
};

const UInt MINSIZE = 1 << 15;
const UInt MAXIND = 3;
const UInt START_INDEX = 20;

typedef array<UInt, MAXIND> t_xs;
const UInt NIL = UInt(-1);
const t_xs NILS = {NIL,NIL,NIL};

struct Clause {
    Clause(): xs(NILS) {}
    UInt len;
    UIntS hgs;
    UInt base;
    UInt neck;
    t_xs xs;

    inline bool match(const t_xs &xm) const {
        for (size_t i = 0; i < MAXIND; i++) {
            UInt x = xm[i];
            UInt y = xs[i];
            if (0 == x || 0 == y)
                continue;
            if (x != y)
                return false;
        }
        return true;
    }

};

struct Spine {
    UInt hd;        // head of the clause to which this corresponds
    UInt base;      // top of the heap when this was created

    UIntList gs;    // goals - with the top one ready to unfold
    UInt ttop;      // top of the trail when this was created

    UInt k;
    t_xs xs;        // index elements
    UIntS cs;       // array of  clauses known to be unifiable with top goal in gs

    Spine(): xs(NILS) {
    }

    Spine(UInt hd, UInt ttop) :
        hd(hd),
        base(0),
        ttop(ttop),
        k(NIL),
        xs(NILS) {
    }

    bool hasGoals() const {
        return gs.size() > 0;
    }
};

struct Tagger {
    const static UInt
        V	= 0x00000000,
        U	= 0x10000000,
        R   = 0x20000000,
        C	= 0x30000000,
        N	= 0x40000000,
        A	= 0x50000000,
        BAD	= 0x70000000,
        MSK	= 0xF0000000,
        TFU = 0x08000000;

    static inline UInt tag(UInt t, UInt w) {
        return t | (w & ~MSK);
    }
    static inline UInt detag(UInt w) {
        return w & ~MSK;
    }
    static inline UInt tagOf(UInt w) {
        return w & MSK;
    }
    static inline bool isVAR(UInt x) {
        auto t = tagOf(x);
        return t == V || t == U;
    }
    static inline Int getInt(UInt x) {
        assert(tagOf(x) == N);
        auto w = detag(x);
        if (w & TFU)
            w |= MSK;   // assume negative
        return Int(w);
    }
};

class Engine: public Tagger {

public:

    Engine(string asm_nl_source);
    virtual ~Engine();

    string stats() const;

protected:

    vector<string> syms;
    vector<Clause> clauses;
    UIntS cls;
    UIntS heap;
    UInt top = NIL;
    UIntStack trail;
    UIntStack ustack;

    vector<Spine> spines;
    size_t spines_top;
    Spine* new_spine(const UIntS& gs0, UInt base, const UIntList &gs, UInt ttop);

    Spine *query = nullptr;

    t_imaps imaps;
    t_vmap vmaps;

    static void pp(string s);

    static UIntS toNums(vector<Clause> clauses);

    static UIntS getSpine(const UIntS &cs);
    static inline UInt relocate(UInt b, UInt cell) {
        UInt t = tagOf(cell);
        return t == V || t == U || t == R ? cell + b : cell;
    }

    UInt addSym(cstr sym);
    cstr getSym(UInt w) const {
        if (w >= syms.size())
            throw logic_error(cstr("BADSYMREF=") + w);
        return syms[w];
    }
    void makeHeap(UInt size = MINSIZE) {
        heap.resize(size);
        clear();
    }
    inline void clear() {
        top = NIL;
    }
    inline void push(UInt i) {
        heap[++top] = i;
    }
    inline UInt size() const {
        return top + 1;
    }
    void expand() {
        heap.resize(heap.size() * 2);
    }
    void ensureSize(UInt more) {
        if (1 + top + more >= heap.size())
            expand();
    }
    vector<Clause> dload(cstr s);

    inline UInt getRef(UInt x) const { return heap[detag(x)]; }
    inline void setRef(UInt w, UInt r) { heap[detag(w)] = r; }
    inline UInt encode(UInt t, cstr s) {
        UInt w = C == t ? addSym(s) : UInt(stol(s));
        return tag(t, w);
    }
    void unwindTrail(UInt savedTop);
    inline UInt deref(UInt x) const {
        while (isVAR(x)) {
            auto r = getRef(x);
            if (r == x)
                break;
            x = r;
        }
        return x;
    }

    /**
     * raw display of a term - to be overridden
     */
    virtual string showTerm(UInt x) const {
      return showTerm(exportTerm(x));
    }

    /**
     * raw display of a externalized term
     */
    virtual string showTerm(Object O) const {
      return O.toString();
    }

    void ppTrail() {
        for (UInt i = 0; i < trail[trail.size()]; i++) {
            auto t = trail[i];
            pp(cstr("trail[") + i + "]=" + showCell(t) + ":" + showTerm(t));
        }
    }
    Object exportTerm(UInt x) const;
    string showCell(UInt w) const;
    string showCells2(UInt base, UInt len) const {
        string buf;
        for (UInt k = 0; k < len; k++) {
            auto instr = heap[base + k];
            buf += cstr("[") + (base + k) + "]" + showCell(instr) + " ";
        }
        return buf;
    }
    string showCells1(UIntS cs) const {
        string buf;
        for (UInt k = 0; k < cs.size(); k++)
            buf += cstr("[") + k + "]" + showCell(cs[k]) + " ";
        return buf;
    }

    bool unify(UInt base);
    bool unify_args(UInt w1, UInt w2);

    Clause putClause(UIntS cs, UIntS gs, UInt neck);

    void pushCells1(UInt b, UInt from, UInt to, UInt base) {
        ensureSize(to - from);
        for (UInt i = from; i < to; i++)
            push(relocate(b, heap[base + i]));
    }
    void pushCells2(UInt b, UInt from, UInt to, const UIntS &cs) {
        ensureSize(to - from);
        for (UInt i = from; i < to; i++)
            push(relocate(b, cs[i]));
    }
    UInt pushHead(UInt b, const Clause& C) {
        pushCells1(b, 0, C.neck, C.base);
        return relocate(b, C.hgs[0]);
    }

    UIntS gs_pushBody;
    void pushBody(UInt b, UInt head, Clause& C);

    void makeIndexArgs(Spine& G);
    void getIndexables(UInt ref, Clause &c) const;
    UInt cell2index(UInt cell) const;

    Spine* unfold();
    size_t c_inferences = 0;

    inline Clause getQuery() const {
        return clauses.back();
    }
    Spine* init();
    inline Spine* answer(UInt ttop) const {
        return new Spine(spines[0].hd, ttop);
    }
    void popSpine();

    Spine* yield_();
    Object ask();

    Toks::Tss vcreate(size_t l) {
        return Toks::Tss(l);
    }

    void put(t_xs& keys, UInt val) {
        for (UInt i = 0; i < imaps.size(); i++) {
            UInt key = keys[i];
            if (key != 0)
                imaps[i][key] = val;
            else
                vmaps[i][val] = val;
        }
    }
    void index(vector<Clause> clauses) {
        if (clauses.size() < START_INDEX)
            return;
        //var T = JSON.stringify
        imaps = t_imaps(vmaps.size());
        for (UInt i = 0; i < clauses.size(); i++) {
            Clause c = clauses[i];
            //pp("!!!xs=" + T(c.xs) + ":" + this.showCells1(c.xs) + "=>" + i)
            put(c.xs, i + 1); // $$$ UGLY INC
            //pp(T(imaps));
        }
        /*
        pp("INDEX");
        pp(T(imaps));
        pp(T(vmaps));
        pp("");
        */
    }
    void ppHeap();

private:
    unordered_map<size_t, size_t> c_spine_mem;
};

class Prog : public Engine {

public:
    Prog(string s);
    ~Prog() override;

    void run(bool print_ans);
    void ppCode() const;

protected:

    string showClause(const Clause &s) const;
    string showTerm(Object O) const override;

    void ppGoals(UIntS bs) const {
        for (auto b: bs) {
            pp(showTerm(exportTerm(b)));
        }
    }
    void ppc(Spine S) const {
        auto bs = S.gs;
        pp(cstr("\nppc: t=") + S.ttop + ",k=" + S.k + "len=" + UInt(bs.size()));
        ppGoals(bs);
    }

    static string maybeNull(const Object &O);
    static inline bool isListCons(cstr name) { return "." == name || "[|]" == name || "list" == name; }
    static inline bool isOp(cstr name) { return "/" == name || "-" == name || "+" == name || "=" == name; }

    static string st0(const vector<Object> &args);
};

}

#endif // HHPROLOG_H
