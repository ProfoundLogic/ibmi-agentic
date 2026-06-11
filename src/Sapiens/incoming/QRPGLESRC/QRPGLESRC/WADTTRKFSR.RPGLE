     H*copyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H* StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/11/10 C:191 Project: 25WRC                                 *
     H*               -Convert to RPGLE                               *
     H*                                                               *
     H* 10/02/06  C:33  Project: WC1196                               *
     H*                -Created.                                      *
     H*****************************************************************
     FWTSSL1    IF   E           K DISK
     FWTAAL1    IF   E           K DISK
     F                                     RENAME(PAA01:PAA01T)
     FWTB3L     IF   E           K DISK
     F                                     RENAME(PB301:PB300)
     FWDRRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDEHL1    IF   E           K DISK
     F                                     RENAME(PEH01:PEH14)
     FWMXHL1    IF   E           K DISK
     F                                     INFDS(XHFDS)
     FWMXHP     UF A E             DISK
     F                                     RENAME(PXH01:PXH0P)
