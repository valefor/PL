#include "Interface.h"
#include <iostream>
#include <boost/regex.hpp>

class Console {

    Operable * oprb;
    Operable * root;

    enum CMD_TYPE { CT_HP, CT_LS, CT_EXC, CT_CD, CT_QT, CT_EX, CT_UNDEF, CT_ERR };
    //enum PRP_LEVEL { PT_DEBUG, PT_NOTICE, PT_WARNING, PT_ERR };
    bool dispatch(CMD_TYPE,boost::smatch &);
    CMD_TYPE cmdFilter(std::string,boost::smatch &);

    public:
    Console(Operable &);
    void start();
    void helper();
};
