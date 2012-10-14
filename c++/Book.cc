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

Book& Book::addChapter(int number,Chapter* chapter) {
    chapters.insert( std::make_pair(number,chapter) );
    return *this;
}
/* C++ Primer chapter class */
Chapter::Chapter(std::string name):chapterName(name) {
}

Chapter& Chapter::addSection(int number,Section* section) {
    sections.insert( std::make_pair(number,section) );
    return *this;
}
/* C++ Primer section class */
Section::Section(std::string name):sectionName(name) {
}

Section& Section::addExercise(int number,Exercise* exercise) {
    exercises.insert( std::make_pair(number,exercise) );
    return *this;
}
/* C++ Primer exercise class */
Exercise::Exercise(int num):number(num),doIt(NULL) {

}

Exercise& Exercise::setDoIt( void (*doFunc)() ) {
    doIt = doFunc;
    return *this;
}
