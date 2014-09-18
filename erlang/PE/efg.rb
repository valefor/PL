#!/usr/bin/ruby -w

# *****************************
# Subject:  Ruby Tool
# Topic :   Erlang File Generator
# Author:   Herr_Alucard(adrain.f.tepes@gmail.com)
# *****************************
class EFG

    # class variable

    def initialize(name=nil)
        @name = name
        @imports = []
    end

    attr_reader :name
    attr_writer :name

    def generate
        %x{
        echo "-module(#@name).\n-export([]).\n-import()." > #@name'.erl'}
    end

end

def show_usage
    print "You must input the filename\n"
    exit
end

unless((ARGV[0].to_s.empty?))
    erl_file_gen = EFG.new(ARGV[0].to_s)
    erl_file_gen.generate
else
    show_usage
end
