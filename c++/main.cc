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

    Console csl = Console(cxxPrimer);
    csl.start();
}

void ex_1_3() {
    std::cout << "Hello world!" << std::endl;
}
