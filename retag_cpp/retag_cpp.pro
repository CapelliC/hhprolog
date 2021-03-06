# hhprolog: Hitchhiker Prolog
#  
# Version: 1.0.0
# License: MIT
# 
# Copyright (c) 2018,2019 Carlo Capelli

TEMPLATE = app

CONFIG += console c++11
CONFIG -= app_bundle qt

SOURCES += \
    main.cpp \
    engine.cpp \
    prog.cpp \
    toks.cpp

HEADERS += \
    hhprolog.h \
    file2string.h

# remove possible other optimization flags
#QMAKE_CXXFLAGS_RELEASE -= -O
#QMAKE_CXXFLAGS_RELEASE -= -O1
#QMAKE_CXXFLAGS_RELEASE -= -O2

# add the desired -O3 if not present
#QMAKE_CXXFLAGS_RELEASE *= -O4
