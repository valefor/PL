#include "Interface.h"

void Operable::add(int index,Operable * opra) {
    opra->parent = this;
    oprbs.insert( std::make_pair(index,opra) );
}

Operable * Operable::cd(int index) {
    OperableMT::iterator it = oprbs.find(index);
    if(it != oprbs.end()) return (Operable *) it->second;

    return NULL;
}

bool Operable::show(int index) {
    OperableMT::iterator it = oprbs.find(index);
    if(it != oprbs.end()) {
        it->second->show();
        return true;
    }

    return false;
}

void Operable::show() {
    OperableMT::iterator iter = oprbs.begin();
    while( iter != oprbs.end() )
    {
        std::cout << iter->first << " - " << iter->second->getName() << std::endl;
        ++iter;
    }
}

Operable::~Operable() {
    OperableMT::iterator iter = oprbs.begin();
    while( iter != oprbs.end() )
    {
        delete iter->second;
        ++iter;
    }
}

