#!/usr/bin/ruby -w

require 'rubygems'
require 'statemachine'

vd_sm = Statemachine.build do
    trans :waiting, :dollar, :paid
    trans :paid, :selection, :waiting
    trans :waiting, :selection, :waiting
    trans :paid, :dollar, :paid
end

puts vd_sm.state
vd_sm.dollar
puts vd_sm.state
vd_sm.selection
puts vd_sm.state

