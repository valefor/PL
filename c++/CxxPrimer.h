#include <string>
#include <map>

class CxxPrimerExercise {
    int number;
    string desc;

};

class CxxPrimerSection {
    map<int,string> index;
    map<int,CxxPrimerExercise*> exercises;

    public:
    CxxPrimerSection();
    CxxPrimerChapter_addSection(int number,string name,CxxPrimerExercise section);
};

class CxxPrimerChapter {
    map<int,string> index;
    map<int,CxxPrimerSection*> sections;

    public:
    CxxPrimerChapter();
    CxxPrimerChapter_addSection(int number,string name,CxxPrimerSection section);
    
};

class CxxPrimer {
    map<int,CxxPrimerChapter*> chapters;

    public:
    CxxPrimer();
    CxxPrimer_addChapter(int number,CxxPrimerChapter chapter);
};

