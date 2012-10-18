#include "Book.h"
#include <iostream>

/* C++ Primer Main class */
Book::Book(std::string name): Operable(name) {
}

void Book::show() {
    std::string upBar = "****************************************";
    std::string leftBar = "* Book Name: ";
    std::cout << upBar << "\n"
              << leftBar << getName() << "\n"
              << upBar << std::endl;
    
    std::cout << "[CHAPTERS]:" << "\n";
    this->Operable::show();
}

void Book::execute() {
    std::cout << "[Book:"<< getName() << "] isn't executable!" << "\n";
}

Book& Book::addChapter(int number,Chapter* chapter) {
    add(number,(Operable *) chapter);
    return *this;
}

/* C++ Primer chapter class */
Chapter::Chapter(std::string name):Operable(name) {
}

Chapter& Chapter::addSection(int number,Section* section) {
    add(number,(Operable *) section);
    return *this;
}

void Chapter::show() {
    std::cout << "[SECTIONS]:" << "\n";
    this->Operable::show();
}

void Chapter::execute() {
    std::cout << "[Chapter:"<< getName() << "] isn't executable!" << "\n";
}

/* C++ Primer section class */
Section::Section(std::string name):Operable(name) {
}

Section& Section::addExercise(int number,Exercise* exercise) {
    add(number,(Operable *) exercise);
    return *this;
}

void Section::show() {
    std::cout << "[EXERCISES]:" << "\n";
    this->Operable::show();
}

void Section::execute() {
    std::cout << "[Section:"<< getName() << "] isn't executable!" << "\n";
}

/* C++ Primer exercise class */
Exercise::Exercise(int num):Operable(),number(num),doIt(NULL) {

}

Exercise& Exercise::setDoIt( void (*doFunc)() ) {
    doIt = doFunc;
    executable = true;
    return *this;
}

void Exercise::show() {
}

void Exercise::execute() {
    if(doIt) doIt();
    else std::cout << "None function has been registered!" << "\n";
}


