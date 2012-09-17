#include <string>
#include <map>


class CxxPrimerExercise {
    int number;
    std::string desc;

    void (*doIt)();

    public:
    CxxPrimerExercise(int);
    void setDoIt( void (*)() );
};

class CxxPrimerSection {
    std::string sectionName;
    std::map<int,CxxPrimerExercise*> exercises;

    public:
    CxxPrimerSection(std::string);
    void addExercise(int number,CxxPrimerExercise* exercise);
};

class CxxPrimerChapter {
    std::string chapterName;
    std::map<int,CxxPrimerSection*> sections;

    public:
    CxxPrimerChapter(std::string);
    void addSection(int number,CxxPrimerSection* section);
    
};

class CxxPrimer {
    std::string bookName;
    std::map<int,CxxPrimerChapter*> chapters;

    public:
    CxxPrimer(std::string);
    void init();
    void show();
    void addChapter(int number,CxxPrimerChapter* chapter);
};

