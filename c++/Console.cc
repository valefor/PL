#include "Console.h"
#include <string>

Console::Console(Operable & opra){
    oprb = &opra;
}  

void Console::start() {

    oprb->show();

    std::string cmd = "";
    boost::smatch matches;

    while (true)
    {
        std::cout << "(csl):";
        std::getline(std::cin,cmd);
        CMD_TYPE ct = cmdFilter(cmd,matches);
        
        std::cout << "command:" << matches[1] << "\n";

        if (!dispatch(ct,matches)) break;
    }
}

Console::CMD_TYPE Console::cmdFilter(std::string cmd,boost::smatch & sm) {
    boost::regex lsRegex("(ls|show|s)");
    boost::regex cdRegex("(goto|g|cd)[ \t]*([0-9])+");
    boost::regex qtRegex("(quit|q|exit)");
    boost::regex undefRegex("([a-zA-Z]+).*");

    if(boost::regex_match(cmd,sm,lsRegex)) return CT_LS;
    if(boost::regex_match(cmd,sm,cdRegex)) return CT_CD;
    if(boost::regex_match(cmd,sm,qtRegex)) return CT_QT;
    //if("ls" == cmd || "show"==cmd || "s" == cmd) return CT_LS;
    //if("goto" == cmd || "g"==cmd || "cd" == cmd) return CT_CD;
    //if("quit" == cmd || "q"==cmd || "exit" == cmd) return CT_QT;

    if(boost::regex_match(cmd,sm,undefRegex)) return CT_UNDEF;

    return CT_ERR;
}

bool Console::dispatch(CMD_TYPE cmdType,boost::smatch & sm) {

    //std::cout << "command:" << cmd << std::endl;
    //if ( CT_QT == cmdType) return false;
    switch(cmdType) {
        case CT_QT: 
            return false;
        case CT_CD:
            break;
        case CT_LS:
            oprb->show();
            break;
        case CT_UNDEF:
            std::cout << "Unrecognized command:" << sm.str(1) <<"\n";
            break;
        default:
            ;
        
    }

    return true;
}

void Console::helper() {
    std::cout << ":cd,ls,ex" << std::endl;
}
