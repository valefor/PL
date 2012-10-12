#include "Book.h"
#include <iostream>

/* C++ Primer Main class */
Book::Book(std::string name): bookName(name) {
}

void Book::show() {
    std::string upBar = "****************************************";
    std::string leftBar = "* Book Name: ";
    std::cout << upBar << "\n"
              << leftBar << bookName << "\n"
              << upBar << std::endl;
}

void Book::addChapter(int number,BookChapter* chapter) {
    chapters.insert( std::make_pair(number,chapter) );
}
/* C++ Primer chapter class */
BookChapter::BookChapter(std::string name):chapterName(name) {
}

void BookChapter::addSection(int number,BookSection* section) {
    sections.insert( std::make_pair(number,section) );
}
/* C++ Primer section class */
BookSection::BookSection(std::string name):sectionName(name) {
}

void BookSection::addExercise(int number,BookExercise* exercise) {
    exercises.insert( std::make_pair(number,exercise) );
}
/* C++ Primer exercise class */
BookExercise::BookExercise(int num):number(num),doIt(NULL) {

}

void BookExercise::setDoIt( void (*doFunc)() ) {
    doIt = doFunc;
}
