/*
 * hhprolog: Hitchhiker Prolog
 * 
 * Version: 1.0.0
 * License: MIT
 * 
 * Copyright (c) 2018,2019 Carlo Capelli
 */

#include <iostream>
#include <chrono>

#include "hhprolog.h"
#include "file2string.h"

using namespace std;

int main(int argc, char *argv[])
{
    try {
        string path = "tests/";
        string fname;
        bool print_ans;

        if (argc == 1) {
            fname = "queens10.pl";
            print_ans = false;
        }
        else {
            fname = argv[1];
            print_ans = argc == 3 ? string(argv[2]) == "true" : false;
        }

        // assume SWI-Prolog already takes care of .pl => .pl.nl
        auto p = new hhprolog::Prog(file2string(path + fname + ".nl"));
        p->ppCode();

        { using namespace chrono;
            auto b = steady_clock::now();
            p->run(print_ans);
            auto e = steady_clock::now();
            cout << "done in " << duration_cast<milliseconds>(e - b).count() << endl;
        }

        cout << p->stats() << endl;

        delete p;
    }
    catch(exception &e) {
        cout << e.what() << endl;
    }
    return 0;
}
