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

    ChapterMT::iterator iter = chapters.begin();
    while( iter != chapters.end() )
    {
        std::cout << iter->first << " - " << iter->second->getName() << std::endl;
        ++iter;
    }
}

Book& Book::addChapter(int number,Chapter* chapter) {
    chapters.insert( std::make_pair(number,chapter) );
    return *this;
}

Operable * Book::cd(int index) {
    
    ChapterMT::iterator it = chapters.find(index);
    if(it != chapters.end()) return (Operable *) it->second;

    return NULL;
}

/* C++ Primer chapter class */
Chapter::Chapter(std::string name):chapterName(name) {
}

Chapter& Chapter::addSection(int number,Section* section) {
    sections.insert( std::make_pair(number,section) );
    return *this;
}

void Chapter::show() {
    SectionMT::iterator iter = sections.begin();
    while( iter != sections.end() )
    {
        std::cout << iter->first << " - " << iter->second->getName() << std::endl;
        ++iter;
    }
}

Operable * Chapter::cd(int index) {

    return NULL;
}

/* C++ Primer section class */
Section::Section(std::string name):sectionName(name) {
}

Section& Section::addExercise(int number,Exercise* exercise) {
    exercises.insert( std::make_pair(number,exercise) );
    return *this;
}

void Section::show() {
}

Operable * Section::cd(int index) {

    return NULL;
}

/* C++ Primer exercise class */
Exercise::Exercise(int num):number(num),doIt(NULL) {

}

Exercise& Exercise::setDoIt( void (*doFunc)() ) {
    doIt = doFunc;
    return *this;
}

void Exercise::show() {
}

Operable * Exercise::cd(int index) {

    return NULL;
}
