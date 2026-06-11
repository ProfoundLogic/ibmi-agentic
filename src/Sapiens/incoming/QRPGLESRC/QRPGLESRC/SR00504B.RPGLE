     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00504B -Retro Three Year Plan Print/Attach program

      *===============================================================*
      * WC000504B: Retrospective Rating Three Year Plan End           *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   "Print/Attach" program for single manual attach endorsement *
      *   with addtional input program that stores information        *
      *   in generic data file WXFMP2.                                *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 10/27/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*

      /COPY SCOPYEN01F

     Fwxfmp2    if   e           k disk    usropn

     Fwxfmp3    o    e             disk    usropn
      /COPY SCOPYEN01D
     D qqForm          s             15A   Inz('WC000504B ')
     D addpgm          c                   const('SR00504I  ')
     d ##llim          s              9  0

     D SR00504B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00504B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform

      /COPY SCOPYEN01C
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr

     C                   open      wxfmp2
     c                   open      wxfmp3
      *    initialize
     c                   movel(p)  xxform        fmform

     C     fmky3         chain     pfm02                              60
      ** Move the custom fields to print
     C                   eval(h)   ##llim = f4llim
     c                   move      *blanks       ##am01           12
     c                   eval      ##am01 =  %editc(##llim:'K':'$')
     c                   move      ##am01        fmc201                         LOSS LIMITIATION
     c                   eval(h)   fma541 = f4lcon                              LOSS CONV. FACTOR
     c                   eval(h)   fma542 = f4minr                              MIN. RETROSPECTIVE
     c                   eval(h)   fma543 = f4maxr                              MAX. RETROSPECTIVE
     c                   eval(h)   fmaM02 = f4e100                              EST. STAND PR..100%
     c                   eval(h)   fma544 = f4b100                              BAS. STAND PR..100%
     c                   eval(h)   fmaM01 = f4e050                              EST. STAND PR..50%
     c                   eval(h)   fma545 = f4b050                              BAS. STAND PR..50%
     c                   eval(h)   fmaM03 = f4e150                              EST. STAND PR..150%
     c                   eval(h)   fma546 = f4b150                              BAS. STAND PR..150%
      **States:
     c                   move      f4st01        fmcd01                         State 1
     c                   move      f4st02        fmcd11                         State 2
     c                   move      f4st03        fmcd21                         State 3
     c                   move      f4st04        fmcd31                         State 4
     c                   move      f4st05        fmcd41                         State 5
     c                   move      f4st06        fmcd42                         State 6
     c                   move      f4st07        fmcd43                         State 7
     c                   move      f4st08        fmcd44                         State 8
     c                   move      f4st09        fmcd45                         State 9
     c                   move      f4st10        fmcd46                         State 10
      **Excess Loss Premium Factors State
      ** First line of screen 2.
     c                   eval(h)   fmrt01 = f4A541                              Generic amount 1
     c                   eval(h)   fmrt02 = f4A542                              Generic amount 2
     c                   eval(h)   fmrt03 = f4A543                              Generic amount 3
     c                   eval(h)   fmrt04 = f4A544                              Generic amount 4
      ** Second line of screen 2.
     c                   eval(h)   fmrt11 = f4A545                              Generic amount 5
     c                   eval(h)   fmrt12 = f4A546                              Generic amount 6
     c                   eval(h)   fmrt13 = f4A547                              Generic amount 7
     c                   eval(h)   fmrt14 = f4A548                              Generic amount 8
      ** Third line of screen 2.
     c                   eval(h)   fmrt21 = f4A549                              Generic amount 10
     c                   eval(h)   fmrt22 = f45410                              Generic amount 11
     c                   eval(h)   fmrt23 = f45411                              Generic amount 12
     c                   eval(h)   fmrt24 = f45412                              Generic amount 13
      ** Fourth line of screen 2.
     c                   eval(h)   fmrt31 = f45413                              Generic amount 13
     c                   eval(h)   fmrt32 = f45414                              Generic amount 14
     c                   eval(h)   fmrt33 = f45415                              Generic amount 15
     c                   eval(h)   fmrt34 = f45416                              Generic amount 16
      ** Fifth line of screen 2.
     c                   eval(h)   fmrt41 = f45417                              Generic amount 17
     c                   eval(h)   fmrt42 = f45418                              Generic amount 18
     c                   eval(h)   fmrt43 = f45419                              Generic amount 19
     c                   eval(h)   fmrt44 = f45420                              Generic amount 20
      ** Sixth line of screen 2.
     c                   eval(h)   fmrt09 = f45421                              Generic amount 21
     c                   eval(h)   fmrt10 = f45422                              Generic amount 22
     c                   eval(h)   fmrt38 = f45423                              Generic amount 23
     c                   eval(h)   fmrt39 = f45424                              Generic amount 25
      ** Seventh line of screen 2.
     c                   eval(h)   fmrt40 = f45425                              Generic amount 26
     c                   eval(h)   fmrt48 = f45426                              Generic amount 27
     c                   eval(h)   fmrt18 = f45427                              Generic amount 28
     c                   eval(h)   fmrt19 = f45428                              Generic amount 29
      ** Eighth line of screen 2.
     c                   eval(h)   fmrt20 = f45429                              Generic amount 30
     c                   eval(h)   fmrt08 = f45430                              Generic amount 31
     c                   eval(h)   fmrt49 = f45431                              Generic amount 32
     c                   eval(h)   fmrt50 = f45432                              Generic amount 32
      ** Ninth line of screen 2.
     c                   eval(h)   fmrt51 = f45433                              Generic amount 32
     c                   eval(h)   fmrt28 = f45434                              Generic amount 34
     c                   eval(h)   fmrt29 = f45435                              Generic amount 35
     c                   eval(h)   fmrt30 = f45436                              Generic amount 36
      ** Tenth line of screen 2.
     c                   eval(h)   fmrt52 = f45437                              Generic amount 37
     c                   eval(h)   fmrt17 = f45438                              Generic amount 38
     c                   eval(h)   fmrt27 = f45439                              Generic amount 39
     c                   eval(h)   fmrt37 = f45440                              Generic amount 40
      *** Retrospective Development Factors(Scrren 4 of display file)
      *First line.
     c                   eval(h)   fmrt05 = f45441                              Generic amount 41

     c                   move      *blanks       ##am02           20
     c                   move      *blanks       ##5442            5 3
     c                   eval(h)   ##5442 = f45442
     c                   eval      ##am02 =  %editc(##5442:'K')
     c                   move      ##am02        fmc205

     c                   move      *blanks       ##am03           20
     c                   move      *blanks       ##5443            5 3
     c                   eval(h)   ##5443 = f45443
     c                   eval      ##am03 =  %editc(##5443:'K')
     c                   move      ##am03        fmc215
      *Second line.
     c                   eval(h)   fmrt15 = f45444                              Generic amount 44

     c                   move      *blanks       ##am02           20
     c                   move      *blanks       ##5445            5 3
     c                   eval(h)   ##5445 = f45445
     c                   eval      ##am02 =  %editc(##5445:'K')
     c                   move      ##am02        fmc206

     c                   move      *blanks       ##am03           20
     c                   move      *blanks       ##5446            5 3
     c                   eval(h)   ##5446 = f45446
     c                   eval      ##am03 =  %editc(##5446:'K')
     c                   move      ##am03        fmc216
      *Third line.
     c                   eval(h)   fmrt25 = f45447                              Generic amount 47

     c                   move      *blanks       ##am02           20
     c                   move      *blanks       ##5448            5 3
     c                   eval(h)   ##5448 = f45448
     c                   eval      ##am02 =  %editc(##5448:'K')
     c                   move      ##am02        fmc207

     c                   move      *blanks       ##am03           20
     c                   move      *blanks       ##5449            5 3
     c                   eval(h)   ##5449 = f45449
     c                   eval      ##am03 =  %editc(##5449:'K')
     c                   move      ##am03        fmc301
      *Fourth line.
     c                   eval(h)   fmrt35 = f45450                              Generic amount 40

     c                   move      *blanks       ##am02           20
     c                   move      *blanks       ##5451            5 3
     c                   eval(h)   ##5451 = f45451
     c                   eval      ##am02 =  %editc(##5451:'K')
     c                   move      ##am02        fmc208

     c                   move      *blanks       ##am03           20
     c                   move      *blanks       ##5452            5 3
     c                   eval(h)   ##5452 = f45452
     c                   eval      ##am03 =  %editc(##5452:'K')
     c                   move      ##am03        fmc302
      *Fifth line.
     c                   eval(h)   fmrt45 = f45453                              Generic amount 53

     c                   move      *blanks       ##am02           20
     c                   move      *blanks       ##5454            5 3
     c                   eval(h)   ##5454 = f45454
     c                   eval      ##am02 =  %editc(##5454:'K')
     c                   move      ##am02        fmc209

     c                   move      *blanks       ##am03           20
     c                   move      *blanks       ##5455            5 3
     c                   eval(h)   ##5455 = f45455
     c                   eval      ##am03 =  %editc(##5455:'K')
     c                   move      ##am03        fmc303
      *Sixth line.
     c                   eval(h)   fmrt06 = f45446                              Generic amount

     c                   move      *blanks       ##am02           20
     c                   move      *blanks       ##5457            5 3
     c                   eval(h)   ##5457 = f45457
     c                   eval      ##am02 =  %editc(##5457:'K')
     c                   move      ##am02        fmc210

     c                   move      *blanks       ##am03           20
     c                   move      *blanks       ##5458            5 3
     c                   eval(h)   ##5458 = f45458
     c                   eval      ##am03 =  %editc(##5458:'K')
     c                   move      ##am03        fmc304
      *Seventh line
     c                   eval(h)   fmrt16 = f45459                              Generic amount

     c                   move      *blanks       ##am02           20
     c                   move      *blanks       ##5460            5 3
     c                   eval(h)   ##5460 = f45460
     c                   eval      ##am02 =  %editc(##5460:'K')
     c                   move      ##am02        fmc211

     c                   move      *blanks       ##am03           20
     c                   move      *blanks       ##5461            5 3
     c                   eval(h)   ##5461 = f45461
     c                   eval      ##am03 =  %editc(##5461:'K')
     c                   move      ##am03        fmc305
      *Eight line
     c                   eval(h)   fmrt26 = f45462                              Generic amount

     c                   move      *blanks       ##am02           20
     c                   move      *blanks       ##5463            5 3
     c                   eval(h)   ##5463 = f45463
     c                   eval      ##am02 =  %editc(##5463:'K')
     c                   move      ##am02        fmc212

     c                   move      *blanks       ##am03           20
     c                   move      *blanks       ##5464            5 3
     c                   eval(h)   ##5464 = f45464
     c                   eval      ##am03 =  %editc(##5464:'K')
     c                   move      ##am03        fmc306
      *Ninth line
     c                   eval(h)   fmrt36 = f45465                              Generic amount

     c                   move      *blanks       ##am02           20
     c                   move      *blanks       ##5466            5 3
     c                   eval(h)   ##5466 = f45466
     c                   eval      ##am02 =  %editc(##5466:'K')
     c                   move      ##am02        fmc213

     c                   move      *blanks       ##am03           20
     c                   move      *blanks       ##5467            5 3
     c                   eval(h)   ##5467 = f45467
     c                   eval      ##am03 =  %editc(##5467:'K')
     c                   move      ##am03        fmc307
      *Tenth line
     c                   eval(h)   fmrt46 = f45468                              Generic amount

     c                   move      *blanks       ##am02           20
     c                   move      *blanks       ##5469            5 3
     c                   eval(h)   ##5469 = f45469
     c                   eval      ##am02 =  %editc(##5469:'K')
     c                   move      ##am02        fmc214

     c                   move      *blanks       ##am03           20
     c                   move      *blanks       ##5470            5 3
     c                   eval(h)   ##5470 = f45470
     c                   eval      ##am03 =  %editc(##5470:'K')
     c                   move      ##am03        fmc308
      ** Write the output record
     C                   write     pfm03

     C                   close     wxfmp2
     C                   close     wxfmp3

     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
