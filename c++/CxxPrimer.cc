#include "CxxPrimer.h"

void CxxPrimer_Init(Book& cxxPrimer)
{
    cxxPrimer
        .addChapter(1, &(new Chapter("Getting Started"))->
            addSection(1, new Section("Writing a Simple C++ Program") )
            .addSection(2, &(new Section("A First Look at Input/Output"))->
               addExercise(3, &(new Exercise("Write a program to print \"Hello, World\" on the standard output."))->setDoIt(ex_1_3) )
               .addExercise(4, new Exercise("Our program used the built-in addition operator, +, to generate the sum of two numbers. Write a program that uses the multiplication operator, *, to generate the product of two numbers") )
               .addExercise(5, new Exercise("We wrote the output in one large statement. Rewrite the program to use a separate statement to print each operand") )
               .addExercise(5, new Exercise("Explain what the following program fragment does") )
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
}

void ex_1_3() {
    std::cout << "Hello world!" << std::endl;
}
