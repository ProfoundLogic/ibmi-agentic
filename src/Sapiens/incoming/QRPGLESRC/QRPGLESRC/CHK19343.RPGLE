     H* (C) Copyright Progressive Data Solutions, 1999
     H/TITLE CHK19343                                                                       *
     H**************************************************************************************
     H*  Description:  Checks employer detail records and determines which policies have   *
     H*                an anniversary date in the State Reproting div but not in at least  *
     H*                one of the other divs.                                              *
     H*                Pass in 'U' to entry parm Update to have the employer detail recs   *
     H*                chgd so that the anniversary dates match.                           *
     H*                                                                                    *
     H*  Called from: Command line                                                         *
     H*  Calls to:    None                                                                 *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 10/29/99   Programmer C:87                                               *
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H**************************************************************************************
     FWMEML     IF   E             DISK                                         emp master
     FWDELL     IF   E           K DISK                                         emp detail
     FWDELL1    UF   E           K DISK    Rename(PEL01:PEL02)                  emp detail
     F                                     Prefix(EL2:2)
     F*
     FQSYSPRT   O    F   80        PRINTER Oflind(*inoa)

     D kkco#           S                   Like(ELCO#)
     D kkfnd           S                   Like(ELFND)
     D kkfyr           S                   Like(ELFYR)
     D kkemp#          S                   Like(ELEMP#)
     D kkdiv           S                   Like(ELDIV)
     D svandt          S                   Like(ELANDT)
     D svdiv           S                   Like(ELDIV)

     C*================================================
     C*  Key fields
     C*================================================
     C     elky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C*
     C     el2ky         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C*
     C*==================================================
     C*  Entry parameters
     C*==================================================
     C     *entry        plist
     C                   parm                    update            1

     C                   except    Head

     C*==================================================
     C*  Main processing
     C*==================================================
     C*
     C*    Read each record in employer master
     C                   read      PEM01                                  60
     C                   dow       *in60 = *off
     C*
     C                   if        EMSTRP = 'Y'
     C*
     C                   eval      kkco# = EMCO#
     C                   eval      kkfnd = EMFND
     C                   eval      kkemp# = EMEMP#
     C                   eval      kkdiv = EMDIV
     C     elky          chain     PEL01                              61
     C                   dow       *in61 = *off
     C*
     C                   if        ELANDT <> 0
     C                   eval       svandt = ELANDT
     C                   eval       svdiv  = ELDIV
     C                   eval       kkfyr  = ELFYR
     C*
     C     el2ky         chain     PEL02                              62
     C                   dow       *in62 = *off
     C*
     C                   if        svdiv <> EL2DIV
     C                   if        svandt <> EL2ANDT
     C                   if        *inoa = *on
     C                   except    Head
     C                   eval       *inoa = *off
     C                   endif                                                  *inoa=*on
     C                   except    Detail
     C                   if        update = 'U'
     C                   eval       EL2ANDT = svandt
     C                   update    PEL02
     C                   endif                                                  update='U'
     C                   endif                                                  svdiv<>EL2DIV
     C                   endif                                                  svandt<>EL2ANDT
     C*
     C     el2ky         reade     PEL02                                  62
     C                   enddo                                                  *in62=*off
     C*
     C                   endif                                                  ELANDT<>0
     C*
     C     elky          reade     PEL01                                  61
     C                   enddo                                                  *in61=*off
     C*
     C                   endif                                                  EMSTRP='Y'
     C*
     C                   read      PEM01                                  60
     C                   enddo
     C*
     C                   eval      *inlr = *on
     O*
     OQSYSPRT   E            Head           1  3
     O                                           77 'Policies that DO NOT have +
     O                                              matching anniversary dates +
     O                                              with main divisions'
     O          E            Head           2
     O                                            6 'Group'
     O                                           17 'Employer'
     O                                           24 'Div'
     O                                           29 'Yr'
     O                                           75 'Page:'
     O                       PAGE
     O          E            Detail         1
     O                       EL2FND        4      6
     O                       EL2EMP#       4     17
     O                       EL2DIV        4     24
     O                       EL2FYR        4     29
