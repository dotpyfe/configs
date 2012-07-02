#!/bin/bash

# flush rules
iptables --flush
iptables --delete-chain

# make some more groups 
iptables -N TCP
iptables -N UDP

# general rules
iptables -P INPUT DROP
iptables -P FORWARD DROP
iptables -P OUTPUT ACCEPT

# allow established and lo
iptables -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
iptables -A INPUT -i lo -j ACCEPT

# dont allow invalid
iptables -A INPUT -m conntrack --ctstate INVALID -j DROP

# allow pings
iptables -A INPUT -p icmp --icmp-type 8 -m conntrack --ctstate NEW -j ACCEPT

# new not syn, with logging!
iptables -A INPUT -m conntrack --ctstate NEW -j LOG --log-level info --log-prefix '*CONN-LOG*'
iptables -A INPUT -p udp -m conntrack --ctstate NEW -j UDP
iptables -A INPUT -p tcp --syn -m conntrack --ctstate NEW -j TCP

# reject tcp rst and udp unreachable
iptables -A INPUT -p udp -j REJECT --reject-with icmp-port-unreachable
iptables -A INPUT -p tcp -j REJECT --reject-with tcp-rst

# default linux behavior
iptables -A INPUT -j REJECT --reject-with icmp-proto-unreachable

# tcp rules
iptables -A TCP -p tcp --dport ssh -j ACCEPT
iptables -A TCP -p tcp --dport http -j ACCEPT
iptables -A TCP -p tcp --dport https -j ACCEPT
iptables -A TCP -p tcp -s 165.124.0.0/16 --dport 8080 -j ACCEPT
iptables -A TCP -p tcp -m mac --mac-source a0:0b:ba:8f:19:1a --dport 8080 -j ACCEPT

# udp rules
iptables -A UDP -p udp -j DROP
