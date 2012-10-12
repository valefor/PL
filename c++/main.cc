#include "Book.h"
#include <iostream>

void ex_1_3();

int main(int argc, char ** argv)
{
    Book cxxPrimer = Book("C++ Primer");

    BookChapter *chapter1  = new BookChapter("Getting Started");
    BookChapter *chapter2  = new BookChapter("Variables and Basic Types");
    BookChapter *chapter3  = new BookChapter("Library Types");
    BookChapter *chapter4  = new BookChapter("Arrays and Pointers");
    BookChapter *chapter5  = new BookChapter("Expressions");
    BookChapter *chapter6  = new BookChapter("Statements");
    BookChapter *chapter7  = new BookChapter("Functions");
    BookChapter *chapter8  = new BookChapter("The IO Library");
    BookChapter *chapter9  = new BookChapter("Sequential Containers");
    BookChapter *chapter10 = new BookChapter("Associative Containers");
    BookChapter *chapter11 = new BookChapter("Generic Algorithms");
    BookChapter *chapter12 = new BookChapter("Classes");
    BookChapter *chapter13 = new BookChapter("Copy Control");
    BookChapter *chapter14 = new BookChapter("Overloaded Operations and Conversions");
    BookChapter *chapter15 = new BookChapter("Object-Oriented Programming");
    BookChapter *chapter16 = new BookChapter("Templates and Generic Programming");
    BookChapter *chapter17 = new BookChapter("Tools for Large Programs");
    BookChapter *chapter18 = new BookChapter("Specialized Tools and Techniques");

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

    BookSection *section1 = new BookSection("Writing a Simple C++ Program");
    BookSection *section2 = new BookSection("A First Look at Input/Output");
    chapter1->addSection(1, section1);
    chapter1->addSection(2, section2);

    BookExercise *exercise1_3 = new BookExercise(3);
    exercise1_3->setDoIt(ex_1_3);
    section2->addExercise(3, exercise1_3);

    cxxPrimer.show();
}

void ex_1_3() {
    std::cout << "Hello world!" << std::endl;
}
