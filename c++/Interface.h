#ifndef OPR_INCLUDED
#define OPR_INCLUDED

class Operable {

    public:
    bool executable;
    virtual bool cd(Operable & oprb) = 0;
    virtual void show() = 0;
    virtual ~Operable() { }

};
#endif
