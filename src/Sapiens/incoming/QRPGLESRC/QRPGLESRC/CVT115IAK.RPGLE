    h* (C) Copyright StoneRiver, Inc. 2009
    H*
    H/TITLE CVT115IAK - SIS Darts Conversion
    H**********************************************************************
    H*                                                                    *
    H* Called From: Command Line                                          *
    H* Calls to   : None                                                  *
    H*                                                                    *
    H**********************************************************************
    H* Created on 04/17/2009 by c:62 for Project 115IAK                   *
    H**********************************************************************
    H* Revisions:                                                         *
    H*                                                                    *
    H**********************************************************************
     Fsdcmlc    if a e           k disk
     Fsdcmle    if a e           k disk    rename(pcm01:pcm02)
     Fsdail     if a e           k disk
     Fsmarl1    if a e           k disk
     Fsmdll2    if a e           k disk
     Fimdola    if a e           k disk
     Fimdolc    if a e           k disk
     Fsmmdl     if a e           k disk
     D                 ds
     D siskey                        40
     D  sisco#                        2    overlay(siskey)
     D  sisprefix                     4    overlay(siskey:3)
     D  sispolicy                     9    overlay(siskey:7)
     D  sisbranch                     2    overlay(siskey:16)
     D  sisclaim                     10    overlay(siskey:18)
    
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     d ##fkey          s             40
     d ##tkey          s             40
     d ##fseq          s              5  0
     d ##tseq          s              5  0
     d ##call          s              1
     d kkpnam          s             10
     d kkkey           s                   like(cmkey)
     d kkseq#          s                   like(cmseq#)
     d hdkey           s                   like(cmkey)
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     cmky          klist
     C                   kfld                    kkpnam
     C*
     C     cmky2         klist
     C                   kfld                    kkkey
     C                   kfld                    cmseq#
     C*
     C     dartsky       klist
     C                   kfld                    hdkey
     C                   kfld                    cmseq#
     C*
     C                   eval      kkpnam='SISCLM'
     C     cmky          chain     pcm01                              99
     C                   dow       not *in99
     C*
     C                   eval      siskey=cmkey
     C                   eval      hdkey=cmkey
     C                   eval      kkkey=sisclaim
     c     cmky2         chain     pcm02                              99
     c                   if        *in99
     C*
     C* Write SDCMP record
     C*
     C                   eval      cmkey=sisclaim
     C                   eval      cmpnam='SISCL2'
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmdt
     C                   z-add     dstime        cmtime
     C                   write     pcm01
     C*
     C* Write SDAIP record
     C*
     C     dartsky       chain     pai01                              99
     C                   dow       not *in99
     C                   eval      aikey=sisclaim
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         aidt
     C                   z-add     dstime        aitime
     C                   write     pai01
     C     dartsky       reade     pai01                                  99
     C                   enddo
     C*
     C* Write SMARP record
     C*
     C     dartsky       chain     par01                              99
     C                   dow       not *in99
     C                   eval      arkey=sisclaim
     C                   eval      arpnam='SISCL2'
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ardt
     C                   z-add     dstime        artime
     C                   write     par01
     C     dartsky       reade     par01                                  99
     C                   enddo
     C*
     C* Write SMDLP record
     C*
     C     dartsky       chain     pdl01                              99
     C                   dow       not *in99
     C                   eval      dlkey=sisclaim
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dldt
     C                   z-add     dstime        dltime
     C                   write     pdl01
     C     dartsky       reade     pdl01                                  99
     C                   enddo
     C*
     C* Write IMDOP record
     C*
     C     dartsky       chain     pdo01                              99
     C                   dow       not *in99
     C                   eval      dodkey=sisclaim
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         doldt
     C                   z-add     dstime        doltim
     C                   write     pdo01
     C     dartsky       reade     pdo01                                  99
     C                   enddo
     C*
     C* Write IMDOP1 record
     C*
     C     dartsky       chain     pdo011                             99
     C                   dow       not *in99
     C                   eval      dodkey=sisclaim
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         doldt
     C                   z-add     dstime        doltim
     C                   write     pdo011
     C     dartsky       reade     pdo011                                 99
     C                   enddo
     C*
     C* Write SMMDP record
     C*
     C     dartsky       chain     pmd01                              99
     C                   dow       not *in99
     C                   eval      mdkey=sisclaim
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         mdldt
     C                   z-add     dstime        mdltim
     C                   write     pmd01
     C     dartsky       reade     pmd01                                  99
     C                   enddo
     C*
     C* Write SMCLP record
     C*
     C                   call      'DA703R'                             99
     C                   parm      siskey        ##fkey
     C                   parm      cmseq#        ##fseq
     C                   parm      sisclaim      ##tkey
     C                   parm      cmseq#        ##tseq
     C                   parm      'C'           ##call
     C*
     C                   endif
     C     cmky          reade     pcm01                                  99
     C                   enddo
     C*
     C                   seton                                        lr
     C*=====================================================================
     C*
     C*=====================================================================
     C     updateF       begsr
     C*
     C*
     C                   endsr
