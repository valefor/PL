#include "CxxPrimer.h"
#include <iostream>

void ex_1_3();

int main(int argc, char ** argv)
{
    CxxPrimer cxxPrimer = CxxPrimer("C++ Primer");

    CxxPrimerChapter *chapter1  = new CxxPrimerChapter("Getting Started");
    CxxPrimerChapter *chapter2  = new CxxPrimerChapter("Variables and Basic Types");
    CxxPrimerChapter *chapter3  = new CxxPrimerChapter("Library Types");
    CxxPrimerChapter *chapter4  = new CxxPrimerChapter("Arrays and Pointers");
    CxxPrimerChapter *chapter5  = new CxxPrimerChapter("Expressions");
    CxxPrimerChapter *chapter6  = new CxxPrimerChapter("Statements");
    CxxPrimerChapter *chapter7  = new CxxPrimerChapter("Functions");
    CxxPrimerChapter *chapter8  = new CxxPrimerChapter("The IO Library");
    CxxPrimerChapter *chapter9  = new CxxPrimerChapter("Sequential Containers");
    CxxPrimerChapter *chapter10 = new CxxPrimerChapter("Associative Containers");
    CxxPrimerChapter *chapter11 = new CxxPrimerChapter("Generic Algorithms");
    CxxPrimerChapter *chapter12 = new CxxPrimerChapter("Classes");
    CxxPrimerChapter *chapter13 = new CxxPrimerChapter("Copy Control");
    CxxPrimerChapter *chapter14 = new CxxPrimerChapter("Overloaded Operations and Conversions");
    CxxPrimerChapter *chapter15 = new CxxPrimerChapter("Object-Oriented Programming");
    CxxPrimerChapter *chapter16 = new CxxPrimerChapter("Templates and Generic Programming");
    CxxPrimerChapter *chapter17 = new CxxPrimerChapter("Tools for Large Programs");
    CxxPrimerChapter *chapter18 = new CxxPrimerChapter("Specialized Tools and Techniques");

    cxxPrimer.addChapter(1, chapter1);
    cxxPrimer.addChapter(2, chapter2);
    cxxPrimer.addChapter(3, chapter3);
    cxxPrimer.addChapter(4, chapter4);
    cxxPrimer.addChapter(5, chapter5);
    cxxPrimer.addChapter(6, chapter6);
    cxxPrimer.addChapter(7, chapter7);
    cxxPrimer.addChapter(8, chapter8);
    cxxPrimer.addChapter(9, chapter9);
    cxxPrimer.addChapter(10,chapter10);
    cxxPrimer.addChapter(11,chapter11);
    cxxPrimer.addChapter(12,chapter12);
    cxxPrimer.addChapter(13,chapter13);
    cxxPrimer.addChapter(14,chapter14);
    cxxPrimer.addChapter(15,chapter15);
    cxxPrimer.addChapter(16,chapter16);
    cxxPrimer.addChapter(17,chapter17);
    cxxPrimer.addChapter(18,chapter18);

    CxxPrimerSection *section1 = new CxxPrimerSection("Writing a Simple C++ Program");
    CxxPrimerSection *section2 = new CxxPrimerSection("A First Look at Input/Output");
    chapter1->addSection(1, section1);
    chapter1->addSection(2, section2);

    CxxPrimerExercise *exercise1_3 = new CxxPrimerExercise(3);
    exercise1_3->setDoIt(ex_1_3);
    section2->addExercise(3, exercise1_3);

    cxxPrimer.show();
}

void ex_1_3() {
    std::cout << "Hello world!" << std::endl;
}
