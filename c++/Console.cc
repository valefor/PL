#include "Console.h"
#include <string>

Console::Console(Operable & opra){
    root = oprb = &opra;
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
        
        //std::cout << "command:" << matches[1] << "\n";

        if (!dispatch(ct,matches)) break;
    }
}

Console::CMD_TYPE Console::cmdFilter(std::string cmd,boost::smatch & sm) {
    boost::regex lsRegex("(ls|show|s)[ \t]*(.*)");
    boost::regex cdRegex("(goto|g|cd)[ \t]*(.*)");
    boost::regex qtRegex("(quit|q|exit)[ \t]*(.*)");
    boost::regex excRegex("(ex|execute)[ \t]*(.*)");
    boost::regex undefRegex("([a-zA-Z]+)[ \t]*(.*)");
    

    if(boost::regex_match(cmd,sm,lsRegex)) return CT_LS;
    if(boost::regex_match(cmd,sm,cdRegex)) return CT_CD;
    if(boost::regex_match(cmd,sm,qtRegex)) return CT_QT;
    if(boost::regex_match(cmd,sm,excRegex)) return CT_EXC;
    //if("ls" == cmd || "show"==cmd || "s" == cmd) return CT_LS;
    //if("goto" == cmd || "g"==cmd || "cd" == cmd) return CT_CD;
    //if("quit" == cmd || "q"==cmd || "exit" == cmd) return CT_QT;

    if(boost::regex_match(cmd,sm,undefRegex)) return CT_UNDEF;

    return CT_ERR;
}

bool Console::dispatch(CMD_TYPE cmdType,boost::smatch & sm) {

    Operable * opr;
    switch(cmdType) {
      case CT_QT: 
        return false;
      case CT_CD:
        if(sm.str(2).empty()) {
          std::cout << "command:" << sm.str(1) << " needs arguments!" << "\n";
        } else {
          const char * arg= sm.str(2).c_str();
          if(std::atoi(arg)) {
            if( ( opr = oprb->cd(std::atoi(arg)) ) ) {
                oprb = opr;
            }
            else {
                std::cout << "cann't find item with index:" << sm.str(2) << "\n";
            }
          } else if(sm.str(2) == "..") {
            if(oprb->getParent()) oprb = oprb->getParent();
            else std::cout << "Have reached root!" << "\n";
          } else if(sm.str(2) == "/") {
            oprb = root;
          } else {
            std::cout << "Invalid argument:" << sm.str(2) << "\n";
          }
        }
        
        break;
      case CT_LS:
        oprb->show();
        break;
      case CT_EXC:
        oprb->execute();
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
