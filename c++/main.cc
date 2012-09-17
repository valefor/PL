#include "CxxPrimer.h"
#include <iostream>

void ex_1_3();

int main(int argc, char ** argv)
{
    CxxPrimer cxxPrimer = CxxPrimer("C++ Primer");

    cxxPrimer.show();

    CxxPrimerChapter *chapter1 = new CxxPrimerChapter("Getting Started");
    cxxPrimer.addChapter(1, chapter1);

    CxxPrimerSection *section1 = new CxxPrimerSection("Writing a Simple C++ Program");
    CxxPrimerSection *section2 = new CxxPrimerSection("A First Look at Input/Output");
    chapter1->addSection(1, section1);
    chapter1->addSection(2, section2);

    CxxPrimerExercise *exercise1_3 = new CxxPrimerExercise(3);
    exercise1_3->setDoIt(ex_1_3);
    section2->addExercise(3, exercise1_3);

}

void ex_1_3() {
    std::cout << "Hello world!" << std::endl;
}
