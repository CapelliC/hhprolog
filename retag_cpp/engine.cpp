/*
 * hhprolog: Hitchhiker Prolog
 * 
 * Version: 1.0.0
 * License: MIT
 * 
 * Copyright (c) 2018,2019 Carlo Capelli
 */

#include "hhprolog.h"

#include <map>
#include <sstream>
#include <numeric>
#include <iostream>
#include <algorithm>

namespace hhprolog {

Engine::Engine(string asm_nl_source) {
    makeHeap();

    clauses = dload(asm_nl_source);
    cls = toNums(clauses);

    query = init();

    //vmaps = vcreate(MAXIND);
    //imaps = index(clauses, vmaps);
}
Engine::~Engine() {
}

string Engine::stats() const {
    ostringstream s;
    s   << heap.capacity() << ' '
        << spines_top << " of " << spines.capacity() << ' '
        << trail.capacity() << ' '
        << ustack.capacity();
    s << " [ ";
    for (auto c: c_spine_mem)
        s << c.first << ':' << c.second << ' ';
    s << ']';
    return s.str();
}

Spine* Engine::init() {
    UInt base = size();
    Clause G = getQuery();

    trail.reserve(10000);
    ustack.reserve(10000);

    spines.resize(10000);
    spines_top = 0;

    gs_pushBody.resize(100);

    return new_spine(G.hgs, base, UIntS(), NIL);
}
Spine* Engine::new_spine(const UIntS& gs0, UInt base, const UIntList &rgs, UInt ttop) {
    auto *sp = &spines[spines_top++];
    sp->hd = gs0[0];
    sp->cs = cls;
    sp->base = base;
    sp->ttop = ttop;
    sp->xs = NILS;
    sp->k = 0;
    // note: cannot reuse G because the last spines.push_back could relocate the array
    auto req_size = gs0.size() - 1 + ( rgs.size() > 0 ? rgs.size() - 1 : 0 );
#if 0
    sp->gs.reserve(req_size);

    for (size_t x = 1; x < gs.size(); ++x)
        sp->gs.push_back(gs[x]);
    for (size_t x = 1; x < rgs.size(); ++x)
        sp->gs.push_back(rgs[x]);
#else
    sp->gs.resize(req_size);
    size_t y = 0;
    for (size_t x = 1; x < gs0.size(); ++x)
        sp->gs[y++] = gs0[x];
    for (size_t x = 1; x < rgs.size(); ++x)
        sp->gs[y++] = rgs[x];
#endif
    //c_spine_mem[req_size]++;
    return sp;
}

/**
 * transforms a spine containing references to choice point and
 * immutable list of goals into a new spine, by reducing the
 * first goal in the list with a clause that successfully
 * unifies with it - in which case places the goals of the
 * clause at the top of the new list of goals, in reverse order
 */
Spine* Engine::unfold() {
    ++c_inferences;

    Spine& G = spines[spines_top - 1];

    UInt ttop = trail.size() - 1;
    UInt htop = top;
    UInt base = htop + 1;

    //cout << c_inferences << ' ' << ttop << ' ' << htop << ' ' << base << endl;

    makeIndexArgs(G);

    size_t last = G.cs.size();
    for (size_t k = G.k; k < last; k++) {
        Clause& C0 = clauses[G.cs[k]];
        if (!C0.match(G.xs))
            continue;
        UInt base0 = base - C0.base;
        UInt b = tag(V, base0);
        UInt head = pushHead(b, C0);
        ustack.clear();
        ustack.push_back(head);
        ustack.push_back(G.gs[0]);
        if (!unify(base)) {
            unwindTrail(ttop);
            top = htop;
            continue;
        }
        pushBody(b, head, C0);
        G.k = k + 1;
        if (gs_pushBody.size() > 1 || G.gs.size() > 1)
            return new_spine(gs_pushBody, base, G.gs, ttop);
        else
            return answer(ttop);
    }
    return nullptr;
}

void Engine::pushBody(UInt b, UInt head, Clause& C) {
    pushCells1(b, C.neck, C.len, C.base);
    auto l = C.hgs.size();
    gs_pushBody.resize(l);
    gs_pushBody[0] = head;
    for (size_t k = 1; k < l; k++) {
        auto cell = C.hgs[k];
        gs_pushBody[k] = relocate(b, cell);
    }
}

bool Engine::unify(UInt base) {
    while (!ustack.empty()) {
        UInt x1 = deref(ustack.back()); ustack.pop_back();
        UInt x2 = deref(ustack.back()); ustack.pop_back();
        if (x1 != x2) {
            UInt w1 = detag(x1);
            UInt w2 = detag(x2);
            if (isVAR(x1)) {
                if (isVAR(x2) && w2 > w1) {
                    heap[w2] = x1;
                    if (w2 <= base)
                        trail.push_back(x2);
                } else {
                    heap[w1] = x2;
                    if (w1 <= base)
                        trail.push_back(x1);
                }
            } else if (isVAR(x2)) {
                heap[w2] = x1;
                if (w2 <= base)
                    trail.push_back(x2);
            } else if (R == tagOf(x1) && R == tagOf(x2)) {
                if (!unify_args(w1, w2))
                    return false;
            } else
                return false;
        }
    }
    return true;
}

bool Engine::unify_args(UInt w1, UInt w2) {
    UInt v1 = heap[w1];
    UInt v2 = heap[w2];
    // both should be A
    UInt n1 = detag(v1);
    UInt n2 = detag(v2);
    if (n1 != n2)
        return false;
    UInt b1 = 1 + w1;
    UInt b2 = 1 + w2;
    for (UInt i = n1 - 1; Int(i) >= 0; i--) {
        UInt i1 = b1 + i;
        UInt i2 = b2 + i;
        UInt u1 = heap[i1];
        UInt u2 = heap[i2];
        if (u1 == u2)
            continue;
        ustack.push_back(u2);
        ustack.push_back(u1);
    }
    return true;
}

void Engine::unwindTrail(UInt savedTop) {
    while (savedTop + 1 < trail.size()) {
        auto href = trail[trail.size() - 1];
        trail.pop_back();
        setRef(href, href);
    }
}

void Engine::pp(string s) {
    cout << s << endl;
}

UInt Engine::addSym(cstr sym) {
    auto I = find(syms.begin(), syms.end(), sym);
    if (I == syms.end()) {
        syms.push_back(sym);
        return syms.size() - 1;
    }
    return static_cast<UInt>(distance(syms.begin(), I));
}

UIntS Engine::getSpine(const UIntS& cs) {
    UInt a = cs[1];
    UInt w = detag(a);
    UIntS rs(w - 1);
    for (UInt i = 0; i < w - 1; i++) {
        UInt x = cs[3 + i];
        UInt t = tagOf(x);
        if (R != t)
            throw logic_error(cstr("*** getSpine: unexpected tag=") + t);
        rs[i] = detag(x);
    }
    return rs;
}

vector<Clause> Engine::dload(cstr s) {
    auto Wsss = Toks::toSentences(s);
    vector<Clause> Cs;
    for (auto Wss: Wsss) {
        map<string, UIntS> refs;
        UIntS cs;
        UIntS gs;
        auto Rss = Toks::mapExpand(Wss);
        UInt k = 0;
        for (auto ws: Rss) {
            UInt l = ws.size();
            gs.push_back(tag(R, k++));
            cs.push_back(tag(A, l));
            for (auto w: ws) {
                if (1 == w.size())
                    w = "c:" + w;
                auto L = w.substr(2);
                switch (w[0]) {
                case 'c':
                    cs.push_back(encode(C, L));
                    k++;
                    break;
                case 'n':
                    cs.push_back(encode(N, L));
                    k++;
                    break;
                case 'v':
                    refs[L].push_back(k);
                    cs.push_back(tag(BAD, k));
                    k++;
                    break;
                case 'h':
                    refs[L].push_back(k - 1);
                    cs[k - 1] = tag(A, l - 1);
                    gs.pop_back();
                    break;
                default:
                    throw logic_error("FORGOTTEN=" + w);
                }
            }
        }

        for (auto kIs: refs) {
            auto Is = kIs.second;
            UInt leader = NIL;
            for (auto j: Is)
                if (A == tagOf(cs[j])) {
                    leader = j;
                    break;
                }
            if (NIL == leader) {
                leader = Is[0];
                for (auto i: Is)
                    if (i == leader)
                        cs[i] = tag(V, i);
                    else
                        cs[i] = tag(U, leader);
            } else
                for (auto i: Is) {
                    if (i == leader)
                        continue;
                    cs[i] = tag(R, leader);
                }
        }
        auto neck = 1 == gs.size() ? cs.size() : detag(gs[1]);
        auto tgs = gs;
        Cs.push_back(putClause(cs, tgs, neck));
    }
    return Cs;
}

Object Engine::ask() {
    query = yield_();
    if (nullptr == query)
        return Object();
    auto ans = answer(query->ttop);
    auto res = ans->hd;
    auto R = exportTerm(res);
    unwindTrail(query->ttop);
    delete ans;

    delete query;
    query = nullptr;

    return R;
}
Spine* Engine::yield_() {
    while (spines_top) {
        auto C = unfold();
        if (nullptr == C) {
            popSpine(); // no matches
            continue;
        }
        if (C->hasGoals())
            continue;
        return C; // answer
    }
    return nullptr;
}

void Engine::popSpine() {
    --spines_top;
    unwindTrail(spines[spines_top].ttop);
    top = spines[spines_top].base - 1;
}

Object Engine::exportTerm(UInt x) const {
    x = deref(x);
    UInt t = tagOf(x);
    UInt w = detag(x);

    switch (t) {
    case C:
        return Object(getSym(w));
    case N:
        return Object(getInt(x));
    case V:
    //case U:
        return Object(cstr("V") + w);
    case R: {
        UInt a = heap[w];
        if (A != tagOf(a))
            throw logic_error(cstr("*** should be A, found=") + showCell(a));
        UInt n = detag(a);
        vector<Object> args;
        UInt k = w + 1;
        for (UInt i = 0; i < n; i++) {
            UInt j = k + i;
            args.push_back(exportTerm(heap[j]));
        }
        return Object(args);
    }
    default:
        throw logic_error(cstr("*BAD TERM*") + showCell(x));
    }
}

string Engine::showCell(UInt w) const {
    UInt t = tagOf(w);
    UInt val = detag(w);
    string s;
    switch (t) {
    case V:
        s = cstr("v:") + val;
        break;
    case U:
        s = cstr("u:") + val;
        break;
    case N:
        s = cstr("n:") + getInt(w);
        break;
    case C:
        s = cstr("c:") + getSym(val);
        break;
    case R:
        s = cstr("r:") + val;
        break;
    case A:
        s = cstr("a:") + val;
        break;
    default:
        s = cstr("*BAD*=") + w;
    }
    return s;
}

UIntS Engine::toNums(vector<Clause> clauses)
{
    UIntS r(clauses.size());
    iota(r.begin(), r.end(), 0);
    return r;
}

void Engine::makeIndexArgs(Spine& G) {
    if (G.xs[0] != NIL)
        return;

    UInt goal = G.gs[0];
    UInt p = 1 + detag(goal);
    UInt n = min(MAXIND, detag(getRef(goal)));
    for (UInt i = 0; i < n; i++) {
        UInt cell = deref(heap[p + i]);
        G.xs[i] = cell2index(cell);
    }
    //if (imaps) throw "IMap TBD";
}

Clause Engine::putClause(UIntS cs, UIntS gs, UInt neck) {
    UInt base = size();
    UInt b = tag(V, base);
    UInt len = cs.size();

    pushCells2(b, 0, len, cs);

    for (size_t i = 0; i < gs.size(); i++)
        gs[i] = relocate(b, gs[i]);
    Clause XC;
    getIndexables(gs[0], XC);
    XC.len = len;
    XC.hgs = gs;
    XC.base = base;
    XC.neck = neck;
    return XC;
}
void Engine::getIndexables(UInt ref, Clause &c) const {
    UInt p = 1 + detag(ref);
    UInt n = detag(getRef(ref));
    for (UInt i = 0; i < MAXIND && i < n; i++) {
        UInt cell = deref(heap[p + i]);
        c.xs[i] = cell2index(cell);
    }
}
UInt Engine::cell2index(UInt cell) const {
    UInt x = 0;
    UInt t = tagOf(cell);
    switch (t) {
    case R:
        x = getRef(cell);
        break;
    case C:
    case N:
        x = cell;
        break;
    }
    return x;
}

void Engine::ppHeap() {

    pp("\nHEAP:\n");
    string h;
    for (size_t i = 0; i < size(); i++) {
        pp(cstr("[") + i + "]:" + showCell(heap[i]));
    }
}

}
