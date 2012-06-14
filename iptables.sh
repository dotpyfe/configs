#!/bin/bash

# flush rules
iptables -F

# make some more groups 
iptables -N TCP
iptables -N UDP
iptables -N INBOUNDCONN

# general rules
iptables -P INPUT DROP
iptables -P FORWARD DROP
iptables -P OUTPUT ACCEPT

# allow established and lo
iptables -A INPUT -p tcp -m conntrack --ctstate RELATED,ESTABLISHED -j INBOUNDCONN
iptables -A INPUT -i lo -j ACCEPT

# log all connections
iptables -A INBOUNDCONN -p tcp -j LOG --log-prefix ' INBOUND TCP ' --log-level 4
iptables -A INBOUNDCONN -p tcp -j ACCEPT

# dont allow invalid
iptables -A INPUT -m conntrack --ctstate INVALID -j DROP

# allow pings
iptables -A INPUT -p icmp --icmp-type 8 -m conntrack --ctstate NEW -j ACCEPT

# new not syn
iptables -A INPUT -p udp -m conntrack --ctstate NEW -j UDP
iptables -A INPUT -p tcp --syn -m conntrack --ctstate NEW -j TCP

# reject tcp rst and udp unreachable
iptables -A INPUT -p udp -j REJECT --reject-with icmp-port-unreachable
iptables -A INPUT -p tcp -j REJECT --reject-with tcp-rst

# default linux behavior
iptables -A INPUT -j REJECT --reject-with icmp-proto-unreachable

# tcp rules
iptables -A TCP -p tcp --dport 22 -j ACCEPT
iptables -A TCP -p tcp --dport 80 -j ACCEPT

# udp rules
iptables -A UDP -p udp -j DROP
