#include <string>
#include <map>
#include "Interface.h"

class Exercise : public Operable {
    int number;
    std::string desc;

    void (*doIt)();

    public:
    Exercise(int);
    Exercise& setDoIt( void (*)() );
    void show();
    Operable * cd(int);
};

class Section : public Operable {
    std::string sectionName;
    std::map<int,Exercise*> exercises;

    public:
    Section(std::string);
    Section & addExercise(int number,Exercise* exercise);
    std::string getName() { return sectionName;}
    void show();
    Operable * cd(int);
};

class Chapter : public Operable {
    std::string chapterName;
    std::map<int,Section*> sections;

    public:
    Chapter(std::string);
    Chapter & addSection(int number,Section* section);
    std::string getName() { return chapterName;}
    void show();
    Operable * cd(int);
    
};

class Book : public Operable {
    std::string bookName;
    std::map<int,Chapter*> chapters;

    public:
    Book(std::string);
    void init();
    void show();
    Operable * cd(int);
    Book & addChapter(int number,Chapter* chapter);
};

typedef std::map<int,Exercise*> ExerciseMT;
typedef std::map<int,Section*> SectionMT;
typedef std::map<int,Chapter*> ChapterMT;
