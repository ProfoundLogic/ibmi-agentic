     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

    H/TITLE LODWTSFP - Load data for SSN/FEIN Default Values
    H*********************************************************************
     H* REVISED:
     H*
     H* 11/24/11 C:282 PROJECT WC2003
     H*                Added values in compile time data array.
     H*
     H* 08/02/11 C:061 PROJECT WC2003
     H*                Created.
     H*
    H*********************************************************************

     Fwtsfl     uf a e           k disk
     Fwtsfp     uf a e           k disk    rename(psf01:psf0p)
     Fsmcol     if   e           k disk

     D #ofFiles        c                   20
     D fileDesc        s             81    dim(#ofFiles) ctdata perrcd(1)

     DfileInfo         ds
     d seq                            7a
     d   blank01                      1a
     d rpid                                like(sfrpid)
     d   blank02                      1a
     d desc                                like(sfdesc)
     d   blank03                      1a
     d flag                                like(sfflag)
     d   blank04                      1a
     d lvl                            3a
     d   blank05                      1a
     d ty01                                like(sfty01)
     d   blank06                      1a
     d ty02                                like(sfty02)
     d   blank07                      1a
     d ty03                                like(sfty03)
     d   blank08                      1a
     d ty04                                like(sfty04)
     d   blank09                      1a
     d ty05                                like(sfty05)
     d   blank10                      1a
     d ty06                                like(sfty06)

     d i1              s              3  0

     D psds          esds                  extname(szq1p)

     D/copy sPrDate


      **************************************************************
      * KLIST
      **************************************************************

     c     sfky          klist
     c                   kfld                    coco#
     c                   kfld                    rpid


      **************************************************************
      * Main
      **************************************************************

     c     *loval        setll     pco01
     c                   read      pco01

     c                   dow       not(%eof)
     C                   exsr      Process
     c                   read      pco01
     c                   enddo

     C                   eval      *inlr = *on


     C*====================================================================
     C* Process each company
     C*====================================================================
     C     Process       begsr

     C     1             do        #ofFiles      i1
     C                   eval      fileInfo = fileDesc(i1)

     C     sfky          chain     psf01

     C                   if        not %found
     C                             and (rpid <> *blanks)
     C                             and (coco# <> 0)
     C                   exsr      WriteRecord
     C                   endif

     C                   enddo
     C*
     C                   endsr


      *====================================================================
      * Write records to WTSFP
      *====================================================================
     C     WriteRecord   begsr

     C                   eval      sfco#  = coco#
     C                   eval      sfrpid = rpid
     C                   eval      sfdesc = desc
     C                   eval      sfflag = flag
     C                   move      lvl           sflvl
     C                   eval      sfty01 = ty01
     C                   eval      sfty02 = ty02
     C                   eval      sfty03 = ty03
     C                   eval      sfty04 = ty04
     C                   eval      sfty05 = ty05
     C                   eval      sfty06 = ty06

     C                   eval      SFUSER = q1user
     C                   eval      SFPGM  = q1pgm
     C                   eval      SFTIME = TimeTo6(SystemTime)
     C                   eval      SFDT   = DateTo7(SystemDate)
     C                   eval      SFIUSR = q1user
     C                   eval      SFIPGM = q1pgm
     C                   eval      SFITIM = TimeTo6(SystemTime)
     C                   eval      SFIDT  = DateTo7(SystemDate)
      *
     C                   write     psf0p
      *
     C                   endsr

** seq#|rpid      |desc                                    |F|lvl|1|2|3|4|5|6|
    1   W8007R     Deductible detail report prompt          4 999 *
    2   W8005R     Fee detail report prompt                 4 999 *
    3   W7324R     Deductible Invoice Reprint               4 999 *             11/24/11
    4   W7935R1    Reimbursable deductible detail report    4 999 *             11/24/11
    5
    6
    7
    8
    9
   10
   11
   12
   13
   14
   15
   16
   17
   18
   19
   20
