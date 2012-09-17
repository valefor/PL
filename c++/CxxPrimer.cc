#include "CxxPrimer.h"
#include <iostream>

/* C++ Primer Main class */
CxxPrimer::CxxPrimer(std::string name): bookName(name) {
}

void CxxPrimer::show() {
    std::string upBar = "****************************************";
    std::string leftBar = "* Book Name: ";
    std::cout << upBar << "\n"
              << leftBar << bookName << "\n"
              << upBar << std::endl;
}

void CxxPrimer::addChapter(int number,CxxPrimerChapter* chapter) {
    chapters.insert( std::make_pair(number,chapter) );
}
/* C++ Primer chapter class */
CxxPrimerChapter::CxxPrimerChapter(std::string name):chapterName(name) {
}

void CxxPrimerChapter::addSection(int number,CxxPrimerSection* section) {
    sections.insert( std::make_pair(number,section) );
}
/* C++ Primer section class */
CxxPrimerSection::CxxPrimerSection(std::string name):sectionName(name) {
}

void CxxPrimerSection::addExercise(int number,CxxPrimerExercise* exercise) {
    exercises.insert( std::make_pair(number,exercise) );
}
/* C++ Primer exercise class */
CxxPrimerExercise::CxxPrimerExercise(int num):number(num) {

}

void CxxPrimerExercise::setDoIt( void (*doFunc)() ) {
    doIt = doFunc;
}
