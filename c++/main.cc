#include "Book.h"
#include "Console.h"
#include <iostream>

void ex_1_3();

int main(int argc, char ** argv)
{
    Book cxxPrimer = Book("C++ Primer");

    cxxPrimer
        .addChapter(1, &(new Chapter("Getting Started"))->
            addSection(1, new Section("Writing a Simple C++ Program") ).
            addSection(2, &(new Section("A First Look at Input/Output"))->
               addExercise(3, &(new Exercise(3))->setDoIt(ex_1_3) ) 
            )
        )
        .addChapter(2, new Chapter("Variables and Basic Types") )
        .addChapter(3, new Chapter("Library Types") )
        .addChapter(4, new Chapter("Arrays and Pointers") )
        .addChapter(5, new Chapter("Expressions") )
        .addChapter(6, new Chapter("Statements") )
        .addChapter(7, new Chapter("Functions") )
        .addChapter(8, new Chapter("The IO Library") )
        .addChapter(9, new Chapter("Sequential Containers") )
        .addChapter(10,new Chapter("Associative Containers") )
        .addChapter(11,new Chapter("Generic Algorithms") )
        .addChapter(12,new Chapter("Classes") )
        .addChapter(13,new Chapter("Copy Control") )
        .addChapter(14,new Chapter("Overloaded Operations and Conversions") )
        .addChapter(15,new Chapter("Object-Oriented Programming") )
        .addChapter(16,new Chapter("Templates and Generic Programming") )
        .addChapter(17,new Chapter("Tools for Large Programs") )
        .addChapter(18,new Chapter("Specialized Tools and Techniques") );
    /*
    Chapter *chapter2  = new Chapter("Variables and Basic Types");
    Chapter *chapter3  = new Chapter("Library Types");
    Chapter *chapter4  = new Chapter("Arrays and Pointers");
    Chapter *chapter5  = new Chapter("Expressions");
    Chapter *chapter6  = new Chapter("Statements");
    Chapter *chapter7  = new Chapter("Functions");
    Chapter *chapter8  = new Chapter("The IO Library");
    Chapter *chapter9  = new Chapter("Sequential Containers");
    Chapter *chapter10 = new Chapter("Associative Containers");
    Chapter *chapter11 = new Chapter("Generic Algorithms");
    Chapter *chapter12 = new Chapter("Classes");
    Chapter *chapter13 = new Chapter("Copy Control");
    Chapter *chapter14 = new Chapter("Overloaded Operations and Conversions");
    Chapter *chapter15 = new Chapter("Object-Oriented Programming");
    Chapter *chapter16 = new Chapter("Templates and Generic Programming");
    Chapter *chapter17 = new Chapter("Tools for Large Programs");
    Chapter *chapter18 = new Chapter("Specialized Tools and Techniques");
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

    Section *section1 = new Section("Writing a Simple C++ Program");
    Section *section2 = new Section("A First Look at Input/Output");
    chapter1->addSection(1, section1);
    chapter1->addSection(2, section2);

    Exercise *exercise1_3 = new Exercise(3);
    exercise1_3->setDoIt(ex_1_3);
    section2->addExercise(3, exercise1_3);
    */

    //cxxPrimer.show();

    Console csl = Console(cxxPrimer);
    csl.start();
}

void ex_1_3() {
    std::cout << "Hello world!" << std::endl;
}
