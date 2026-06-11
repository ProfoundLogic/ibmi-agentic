     H* (C) Copyright Progressive Data Solutions, 2000
     H/TITLE CVT23658                                                                       *
     H**************************************************************************************
     H*  Description:  Program looks will read the first occurance of                      *
     H*                the claim history file to look for the closed                       *
     H*                rcds first.                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 07/25/00   Programmer C:53    Project: 23658                             *
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H**************************************************************************************
     FWMCHL     UF   E           K DISK
     FQSYSPRT   O    F   80        PRINTER Oflind(*inoa)
     D xxhdr           S                   Like(*inoa)
     D xxtime          S                   Like(chtime)
     D xxefdt          S                   Like(chefdt)
     C*
     C     chky          klist
     C                   kfld                    chco#
     C                   kfld                    chfnd
     C                   kfld                    chfyr
     C                   kfld                    chcase
     C*
     C     *entry        plist
     C                   parm                    update            1
     C*
     C                   exsr      header
     C                   eval      xxhdr = *on

     C                   read      PCH01                                  60
     C*
     C                   dow       *in60 = *off
     C*
     C                   if        chocst = 'C'
     C   oa              exsr      header
     C                   except    detail
     C                   eval      xxtime = chtime - 1
     C                   eval      xxefdt = chefdt
temp C     chky          reade     PCH01                                  60
     C*
     C                   if        *in60 = *off and xxefdt = chefdt
     C                   eval      chtime = xxtime
temp C                   except    detail
     C*
     C                   if        update = 'Y'
     C                   update    PCH01
     C                   endif
     C                   endif
     C*
temp C                   except    skip
     C                   endif
     C*
     C     chky          setgt     PCH01
     C                   read      PCH01                                  60
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      *inlr = *on
     C*
     Csr   header        begsr
     Csr                 except    head
     Csr                 eval      *inoa = *off
     Csr                 endsr
     C*
     OQSYSPRT   E            Head           1  3
     O                                            9 'CVT23658'
     O                                           67 'Claim history rcds with t+
     O                                              he initial status being cl+
     O                                              osed'
     O                                           75 'Page:'
     O                       PAGE
     O          E            Head        1  2
     O                                            6 'CO#'
     O                                           17 'FUND'
     O                                           24 'FYR'
     O                                           35 'CASE'
     O                                           41 'STAT'
     O                                           45 'PGM'
     O                                           60 'EF DATE'
     O                                           70 'TIME'
     O          E            Detail         1
     O                       chco#         4      6
     O                       chfnd         4     17
     O                       chfyr         4     24
     O                       chcase        4     35
     O                       chocst              39
     O                       chpgm               52
     O                       chefdt              60
     O                       chtime              70
     O          E            skip           1
