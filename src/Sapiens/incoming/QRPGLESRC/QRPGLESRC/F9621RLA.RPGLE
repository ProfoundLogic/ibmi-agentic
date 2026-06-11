    H*****************************************************************
    H** (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC., 2002
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*****************************************************************
    H*    PROGRAM: F9621RLA                                          *
    H*    DESCRIPTION: BUILD WORKFILE - LA DATA SUBMISSION           *
    H*                                                               *
    H*    CALLED FROM:                                               *
    H*      F9621CLA                                                 *
    H*****************************************************************
    H** CREATED:                                                     *
    H*  08/02/00 C:79 Proj: LA014 Convert to Provider Bills          *
    H*****************************************************************
    H** REVISIONS:                                                   *
     H*                                                               *
     H*  03/18/02 C:62 PROJECT MC010 31686                            *
     H*                -Expand xxage to 4.0                           *
     H*                                                               *
     H*  02/08/02 C:62 PROJECT LA018                                  *
     H*                -EXPAND TOS.                                   *
     H*                -ADD ADDITIONAL TYPE OF SERVICE CODES.         *
     H*                -SWAP HOSPITAL TOS CODES.                      *
     H*                                                               *
    H*****************************************************************
    H*  INDICATOR USAGE:                                             *
    H*****************************************************************
     Ffdm2pla   o    e             disk
     Ffdh2pla   o    e             disk
     Ffaeol     if   e             disk
     Ffaeul     if   e           k disk
     Ffddhl     if   e           k disk
     Ffddul     if   e           k disk
     Ffmc1l     if   e           k disk
     Ffmcdl     if   e           k disk
     Ffdcalh    if   e           k disk
     Ffmprlla   if   e           k disk
     Fsmupl     if   e           k disk
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D                 ds
     D xxdate                         8  0
     D  xxmd                          4  0 overlay(xxdate)
     D  xxyy                          4  0 overlay(xxdate:5)
     D                 ds
     D yydate                         8  0
     D  yymd                          4  0 overlay(yydate)
     D  yyyy                          4  0 overlay(yydate:5)
     D                 ds
     D zzdate                         8  0
     D  zzmd                          4  0 overlay(zzdate)
     D  zzyy                          4  0 overlay(zzdate:5)
    D*
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D kkcase          s                   like(eocase)
     D kkco#           s                   like(eoco#)
     D kkcpt4          s              5
     D kkeob#          s                   like(eoeob#)
     D kkdseq          s                   like(cadseq)
     D kkfdat          s              7  0
     D kkfnd           s                   like(eofnd)
     D kkfyr           s                   like(eofyr)
     D kkss#           s                   like(cass#)
     D kksub#          s                   like(eosub#)
     D qqco#           s              3
     D qqsys           s              1
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xxage           s              3  0
     D xxanam          s              3  0
     D xxanmn          s              3  0
     D xxdays          s              5  0
     D xxdt            s              8  0
     D xxerr           s              1
     D xxfrdt          s              7  0
     D xxlos           s              3  0
     D xxpmod          s              3  0
     D xxtodt          s              7  0
     D xxunts          s              3  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*================================================================
    C*   PLISTs
    C*================================================================
    C*
    C*  Entry Parameters
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqsys
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     futr8         plist
     C                   parm                    xxdt
     C                   parm                    xxdays
     C                   parm                    xxerr
    C*
    C*================================================================
    C*   KLISTs
    C*================================================================
    C*
    C* Key List for the Company Master File
    C*
     C     cokey         klist
     C                   kfld                    upco#
    C*
    C* Key List for the User Parameter Master File
    C*
     C     upkey         klist
     C                   kfld                    q1user
    C*
    C* Key List for the Fee Schedule Company Auxiliary File
    C*
     C     c1ky          klist
     C                   kfld                    kkco#
    C*
    C* Key List for the Claim File
    C*
     C     cdky          klist
     C                   kfld                    kkss#
    C*
    C* Key List for the hospital header file
    C*
     C     euky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
    C* Key List for the Fee Schedule Case Detail File
    C*
     C     caky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
    C* Key List for the Fee Schedule physician detail file
    C*
     C     dhky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
    C*
    C* Key List for the Fee Schedule hospital detail file
    C*
     C     duky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
    C*
    C* Key List for the Procedure Code File (LA)
    C*
     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4
     C                   kfld                    kkfdat
    C*
    C*================================================================
    C*   *LIKE DEFN Statements
    C*================================================================
    C*
    C*  GET THE USER PARAMETER FILE
    C*
     C     upkey         chain     pup01                              60
     C                   if        *in60 = *on
     C                   clear                   upco#
     C                   endif
    C*
     C                   time                    dsdttm
    C*
     C     qqsys         caseq     'P'           srphys
     C     qqsys         caseq     'H'           srhosp
     C                   endcs
    C*
    C*  End of Program
    C*
     C                   move      *on           *inlr
    C*
    C*================================================================
    C*   SRPHYS - Subroutine for Physician
    C*================================================================
     C     srphys        begsr
    C*
    C*  Read record from QRY
    C*
     C                   read      peo01                                  99
     C                   dow       *in99 = *off
    C*
    C*  Clear all fields
    C*
     C                   clear                   m2prcd
     C                   clear                   m2pmod
     C                   clear                   m2duos
     C                   clear                   m2chga
     C                   clear                   m2tos
     C                   clear                   m2pzip
     C                   clear                   m2dos
     C                   clear                   m2prnm
     C                   clear                   m2dcon
    C*
     C                   movel     eoco#         kkco#
     C                   movel     eosub#        kksub#
     C                   movel     eofnd         kkfnd
     C                   movel     eofyr         kkfyr
     C                   movel     eocase        kkcase
     C                   movel     eoeob#        kkeob#
    C*
     C     caky          chain     pca01                              60
     C                   if        *in60 = *off
    C*
     C                   movel     cadseq        kkdseq
    C*
     C     dhky          chain     pdh01                              60
     C                   movel     dhprcd        m2prcd
     C                   if        dhmod1<>*blanks
     C                   move(p)   dhmod1        m2pmod
     C                   else
     C                   if        dhmod2<>*blanks
     C                   move(p)   dhmod2        m2pmod
     C                   else
     C                   if        dhmod3<>*blanks
     C                   move(p)   dhmod3        m2pmod
     C                   else
     C                   if        dhmod4<>*blanks
     C                   move(p)   dhmod4        m2pmod
     C                   endif
     C                   endif
     C                   endif
     C                   endif
    C*
    C* Following SR is to calculate the units if needed
    C*
     C                   exsr      srunts
    C*
     C                   movel     cachrg        m2chga
     C                   move(p)   cafdat        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel(p)  srdto         m2dos
     C                   movel     cass#         kkss#
     C                   endif
    C*
    C*  The following gets Type of Service
    C*
     C*** BEGIN CHG *** 02/08/02 *******************************************
     C**********         movel     '0'           m2tos
     C                   movel     '00'          m2tos
     C*** END   CHG *** 02/08/02 *******************************************
    C*
     C                   if        dhprcd >= '00000'
     C                             and dhprcd <= '69999'
     C                             and m2pmod = '47'
     C*** BEGIN CHG *** 02/08/02 *******************************************
     C**********         movel     '1'           m2tos
     C                   movel     '01'          m2tos
     C*** END   CHG *** 02/08/02 *******************************************
     C                   endif
     C                   if        dhprcd >= '00100'
     C                             and dhprcd <= '01999'
     C*** BEGIN CHG *** 02/08/02 *******************************************
     C**********         movel     '1'           m2tos
     C                   movel     '01'          m2tos
     C*** END   CHG *** 02/08/02 *******************************************
     C                   endif
     C                   if        dhprcd >= '10000'
     C                             and dhprcd <= '69999'
     C                             and m2pmod <> '47'
     C*** BEGIN CHG *** 02/08/02 *******************************************
     C**********         movel     '2'           m2tos
     C                   movel     '02'          m2tos
     C*** END   CHG *** 02/08/02 *******************************************
     C                   endif
     C                   if        dhprcd >= 'D0000'
     C                             and dhprcd <= 'D9999'
     C*** BEGIN CHG *** 02/08/02 *******************************************
     C**********         movel     '3'           m2tos
     C                   movel     '03'          m2tos
     C*** END   CHG *** 02/08/02 *******************************************
     C                   endif
     C*** BEGIN CHG *** 02/08/02 *******************************************
     C                   if        dhprcd >= 'A4000'
     C                             and dhprcd <= 'A9999'
     C                             or dhprcd >= 'E0000'
     C                             and dhprcd <= 'L9999'
     C                             or dhprcd >= 'Q0000'
     C                             and dhprcd <= 'Q9999'
     C                             or dhprcd >= 'V0000'
     C                             and dhprcd <= 'V9999'
     C*
     C                   select
     C                   when      m2pmod = 'PU'
     C                   eval      m2tos = '20'
     C                   when      m2pmod = 'RT'
     C                   eval      m2tos = '30'
     C                   other
     C                   eval      m2tos = '10'
     C                   endsl
     C*
     C                   endif
     C*** END   CHG *** 02/08/02 *******************************************
    C*
     C     c1ky          chain     pc101                              60
     C                   if        *in60 = *off
     C                   movel     c110a1        m2dcon
     C                   endif
    C*
     C                   movel     eozip         m2pzip
     C                   movel     eopnam        m2prnm
    C*
     C                   write     pm201
    C*
     C                   read      peo01                                  99
    C*
     C                   enddo
    C*
     C                   endsr
    C*================================================================
    C*   SRHOSP - Subroutine for Hospital
    C*================================================================
     C     srhosp        begsr
    C*
    C*  Read record from QRY
    C*
     C                   read      peo01                                  99
     C                   dow       *in99 = *off
    C*
    C*  Clear all fields
    C*
     C                   clear                   h2prnm
     C                   clear                   h2pzip
     C                   clear                   h2addt
     C                   clear                   h2dsdt
     C                   clear                   h2tos
     C                   clear                   h2prdc
     C                   clear                   h2sdc1
     C                   clear                   h2sdc2
     C                   clear                   h2sdc3
     C                   clear                   h2sdc4
     C                   clear                   h2sdc5
     C                   clear                   h2prpc
     C                   clear                   h2spc1
     C                   clear                   h2spc2
     C                   clear                   h2spc3
     C                   clear                   h2spc4
     C                   clear                   h2spc5
     C                   clear                   h2age
     C                   clear                   h2sex
     C                   clear                   h2dcsc
     C                   clear                   h2los
     C                   clear                   h2chga
     C                   clear                   h2dcon
    C*
     C                   movel     eoco#         kkco#
     C                   movel     eosub#        kksub#
     C                   movel     eofnd         kkfnd
     C                   movel     eofyr         kkfyr
     C                   movel     eocase        kkcase
     C                   movel     eoeob#        kkeob#
    C*
     C     caky          chain     pca01                              60
     C                   if        *in60 = *off
    C*
     C                   movel     cadseq        kkdseq
    C*
     C     duky          chain     pdu01                              60
    C*
     C                   movel     cass#         kkss#
     C                   if        durvcd = 001
     C                   movel     '1'           h2tos
     C                   endif
    C*
     C                   if        durvcd = 002
     C                   movel     '2'           h2tos
     C                   endif
    C*
     C                   if        durate = 'PSYCH'
     C                   movel     '3'           h2tos
     C                   endif
    C*
     C                   if        durate = 'CHEMD'
     C                   movel     '4'           h2tos
     C                   endif
    C*
     C                   if        durate = 'REHFS'
     C                   movel     '5'           h2tos
     C                   endif
    C*
     C*** BEGIN CHG *** 02/08/02 *******************************************
     C**********         if        durate = 'SNFFS'
     C**********         movel     '6'           h2tos
     C**********         endif
    C*
     C**********         if        durate = 'SNFHB'
     C**********         movel     '7'           h2tos
     C**********         endif
     C                   if        durate = 'SNFHB'
     C                   movel     '6'           h2tos
     C                   endif
    C*
     C                   if        durate = 'SNFFS'
     C                   movel     '7'           h2tos
     C                   endif
     C*** END   CHG *** 02/08/02 *******************************************
    C*
     C                   if        durate = 'ICFHB'
     C                   movel     '8'           h2tos
     C                   endif
    C*
     C                   if        durate = 'ICFFS'
     C                   movel     '9'           h2tos
     C                   endif
    C*
     C                   if        durate = 'HCHBC'
     C                             or durate = 'HCHBG'
     C                             or durate = 'HCHBR'
     C                             or durate = 'HCHBS'
     C                   movel     '10'          h2tos
     C                   endif
    C*
     C                   if        durate = 'HCFSC'
     C                             or durate = 'HCFSG'
     C                             or durate = 'HCFSR'
     C                             or durate = 'HCFSS'
     C                   movel     '11'          h2tos
     C                   endif
    C*
     C                   endif
    C*
     C     c1ky          chain     pc101                              60
     C                   if        *in60 = *off
     C                   movel     c110a1        h2dcon
     C                   endif
    C*
     C     euky          chain     peu01                              60
     C                   if        *in60 = *off
     C                   if        euaddt <> *zeros
     C                   move      euaddt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         h2addt
     C                   endif
    C*
     C                   movel     eudia5        h2sdc4
     C                   movel     eudia6        h2sdc5
     C                   movel     euprc1        h2prpc
     C                   movel     euprc2        h2spc1
     C                   movel     euprc3        h2spc2
     C                   movel     euprc4        h2spc3
     C                   movel     euprc5        h2spc4
     C                   movel     euprc6        h2spc5
    C*
     C                   if        eupats >= '01' and eupats <= '08'
     C                   eval      h2dcsc=eupats
     C                   endif
    C*
     C                   if        eupats >= '20'
     C                             and eupats <= '29'
     C                             or eupats >= '40'
     C                             and eupats <= '42'
     C                   movel     '09'          h2dcsc
     C                   endif
    C*
     C                   if        eupats >= '30'
     C                             and eupats <= '39'
     C                   movel     '10'          h2dcsc
     C                   endif
    C*
     C                   endif
    C*
     C                   movel     eozip         h2pzip
     C                   movel     eopnam        h2prnm
     C                   movel     eudia1        h2prdc
     C                   movel     eudia2        h2sdc1
     C                   movel     eudia3        h2sdc2
     C                   movel     eudia4        h2sdc3
    C*
    C* Following SR calcs Length of Stay
    C*
     C                   exsr      srlos
    C*
    C* Follwing pgm calculates the Discharge Date
    C*
     C                   if        h2addt <> *blanks
     C                   move(p)   h2los         xxdays
     C                   eval      xxdays = xxdays - 1
     C                   move(p)   h2addt        xxdt
     C                   clear                   xxerr
     C                   call      'SRFUTR8'     futr8
     C                   if        *in99 = *off
     C                   move(p)   xxdt          h2dsdt
     C                   endif
     C                   endif
    C*
     C                   movel     eochrg        h2chga
    C*
     C     cdky          chain     pcd01                              60
     C                   if        *in60 = *off
    C*
    C* Following SR calcs Age
    C*
     C                   exsr      srage
    C*
     C                   movel     cdsex         h2sex
    C*
     C                   endif
    C*
     C                   write     ph201
    C*
     C                   read      peo01                                  99
    C*
     C                   enddo
    C*
     C                   endsr
    C*================================================================
    C*   SRAGE - Subroutine for Calculating Age
    C*================================================================
     C     srage         begsr
    C*
     C                   eval      srdti = cddtob
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdate
    C*
     C                   eval      zzdate = dsdate
    C*
     C*** BEGIN ADD *** 03/18/02 *******************************************
     C                   if        xxyy <> 0
     C*** END   ADD *** 03/18/02 *******************************************
     C                   eval      xxage = zzyy - xxyy
     C                   if        zzmd < xxmd
     C                   eval      xxage = xxage - 1
     C                   endif
     C                   move      xxage         h2age
     C*** BEGIN ADD *** 03/18/02 *******************************************
     C                   else
     C                   move      xxyy          h2age
     C                   endif
     C*** END   ADD *** 03/18/02 *******************************************
    C*
     C                   endsr
    C*================================================================
    C*   SRLOS - Subroutine for Calculating Length of Stay
    C*================================================================
     C     srlos         begsr
     C                   movel     eotodt        xxtodt
    C*
     C                   move      eofrdt        xxfrdt
    C*
     C                   eval      xxlos = xxtodt - xxfrdt
     C                   eval      xxlos = xxlos + 1
     C                   movel     xxlos         h2los
    C*
     C                   endsr
    C*================================================================
    C*   SRUNTS - Subroutine for Calculating Units
    C*================================================================
     C     srunts        begsr
    C*
     C                   if        dhprcd >= '00100'
     C                             and dhprcd <= '01999'
    C*
    C*  Get anesthesia minutes
    C*
     C                   eval      xxanmn = dhunts / 15
    C*
    C*  Get modifier units if available
    C*
     C                   if        m2pmod = 'P3'
     C                   eval      xxpmod = 1
     C                   endif
     C                   if        m2pmod = 'P4'
     C                   eval      xxpmod = 2
     C                   endif
     C                   if        m2pmod = 'P5'
     C                   eval      xxpmod = 3
     C                   endif
    C*
    C*  Get base units by chaining to FMPRLLA file
    C*
     C                   movel     dhprcd        kkcpt4
     C                   movel     cafdat        kkfdat
     C     prky          setgt     ppr01
     C                   readp     ppr01                                  99
     C                   if        *in99 = *off
     C                   eval      xxanam = pranam
     C                   endif
    C*
    C*  Add them all up!
    C*
     C                   eval      xxunts = xxanam
     C                   eval      xxunts = xxunts + xxanmn
     C                   eval      xxunts = xxunts + xxpmod
     C                   movel     xxunts        m2duos
    C*
     C                   else
     C                   movel     caunts        m2duos
     C                   endif
    C*
     C                   endsr
