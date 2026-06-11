      ** (C) Copyright Progressive Data Solutions, Inc., 1999
     h/title POSTLOAD: Post Load Program for Tape on 10/25/99
      **============================================================**
      ** Program Description:                                       **
      **                                                            **
      **                                                            **
      **                                                            **
      ** Called from:                                               **
      **                                                            **
      ** Calls to:    None                                          **
      **                                                            **
      ** Programmer: C:113                                          **
      ** Created   : 10/19/99                                       **
      ** Project   : CM027                                          **
      **============================================================**
     hdatedit(*ymd) datfmt(*ymd)

     fsmorl     uf a e           k disk                                         Output Routing File
     fmmopp     uf   e             disk                                         Menu Option File
     fwtenp     uf   e             disk                                         End. Table File
     fwmmup     uf   e             disk                                         Attached End Table

      *=============================================================================================
      *  Field Definitions
      *=============================================================================================
     d##DateIn         s              8  0
     d##FromFmt        s              8
     d##ToFmt          s              8
     d##DateOut        s             10
     d##Error          s              1

     dxxProgram        s             10    inz(*blanks)
     dxxW7511R         s             10    inz('W7511R   ')
     dxxW7051R1        s             10    inz('W7051R1  ')
     dxxW1053R1        s             10    inz('W1053R1  ')
     dxxFileNum        s              1a   inz('0')
     d xxProg          s              6a   inz(*blanks)
     dxxDate           s              8s 0

     dxxWorkStor       ds
     dxxTime                          6s 0                                         Time
     d xxHour                         2s 0 overlay(xxTIME:1)                         Hour
     d xxMin                          2s 0 overlay(xxTIME:3)                         Minute
     d xxSec                          2s 0 overlay(xxTIME:5)                         Second

      ** External data structure
     dpsds           esds                  extname(szq1p)                       External prog info
      *=============================================================================================
      *  klist definitions
      *=============================================================================================
     c     orKey         klist
     c                   kfld                    xxProgram
     c                   kfld                    xxFileNum
      *=============================================================================================
      *  Main
      *=============================================================================================
     c                   Eval      xxDate = *date
     c                   time                    xxTime

     c                   Exsr      $Inact                                       Inact. End. Table

     c                   Eval      xxProgram = xxW7051R1                        Midterm Routing
     c                   Exsr      $Routing

     c                   Eval      xxProgram = xxW1053R1                        End Copy Routing
     c                   Exsr      $Routing

     c                   Eval      xxProgram = xxW7511R                         Cert Hold Routing
     c                   Exsr      $Routing

     c                   Exsr      $EndTable
     c                   Exsr      $AttTable

     c                   SetOn                                        lr
     c                   Return
      *=============================================================================================
      * $AttTable - Adjust the Employer Attached Endorsement File
      *=============================================================================================
     c     $AttTable     BegSR

     c                   Read      pmu01                                  99
     c                   DoW       *in99 = *off

     c                   If        MUFORM = 'WC890600  '
     c                   Eval      MUFORM = 'WC890600A '
     c                   Update    pmu01
     c                   EndIf

     c                   Read      pmu01                                  99
     c                   EndDo

     c                   EndSR                                                  $AttTable
      *=============================================================================================
      * $EndTable - Adjust the Endorsement Table
      *=============================================================================================
     c     $EndTable     BegSR

     c                   Read      pen01                                  99
     c                   DoW       *in99 = *off

     c                   If        ENFORM = 'WC890600  '
     c                   Eval      ENFORM = 'WC890600A '
     c                   Update    pen01
     c                   EndIf

     c                   Read      pen01                                  99
     c                   EndDo

     c                   EndSR                                                  $EndTable
      *=============================================================================================
      * $Inact - Inactivate the Work With Endorsement Forms entry
      *=============================================================================================
     c     $Inact        BegSR

     c                   Read      pop01                                  60
     c                   DoW       *in60 = *off

     c                   Eval      xxProg = opexcl
     c                   If        xxProg = 'W1055R'
     c                   Eval      opstat = 'I'
     c                   Update    pop01
     c                   EndIf

     c                   Read      pop01                                  60
     c                   EndDo

     c                   EndSR                                                  $Inact
      *=============================================================================================
      * $Routing - Add routing records for Midterm Endorsements and Endorsement Mass Copy
      *=============================================================================================
     c     $Routing      BegSR

     c     orKey         Chain     smorl                              60

     c                   If        *in60 = *off

     c                   If        orhold <> '*YES' OR
     c                             orcopy <> 01
     c                   Eval      orhold = '*YES'
     c                   Eval      orcopy = 01
     c                   Update    por01
     c                   EndIf                                                  orhold <> '*YES' OR

     c                   Else
     c                   Eval      orpnam = xxProgram
     c                   Eval      orfil# = xxFileNum
     c                   Eval      oroutq = 'QPRINT'
     c                   Eval      orcopy = 01

     c                   Select
     c                   When      xxProgram = xxW7051R1
     c                   Eval      ortitl = 'MIDTERM ENDORSEMENT'

     c                   When      xxProgram = xxW1053R1
     c                   Eval      ortitl = 'ENDORSEMENT MASS COPY'

     c                   When      xxProgram = xxW7511R
     c                   Eval      ortitl = 'CERTIFICATE HOLDERS LETTER'

     c                   EndSl

     c                   Eval      orsave = '*NO'
     c                   Eval      orhold = '*YES'
     c                   Eval      orform = '*STD'
     c                   Eval      orstat = 'A'
     c                   Eval      orofld = 'OVRPRTF FILE(QSYSPRT) OUTQ(QPRINT)+
     c                                       FORMTYPE(*STD) COPIES(01) HOLD(*YE+
     c                                      S) SAVE(*NO)'
     c                   Eval      oruser = q1user
     c                   Eval      orpgm = q1pgm

     c                   Call      'SRCVTDAT'
     c                   Parm      xxDate        ##DateIn
     c                   Parm      '*CYMD'       ##FromFmt
     c                   Parm      '*YMD'        ##ToFmt
     c                   Parm      *blanks       ##DateOut
     c                   Parm      *blanks       ##Error

     c                   If        ##Error = *off
     c                   Move      ##DateOut     ordt
     c                   EndIf

     c                   Eval      ortime = xxTime
     c                   Write     por01

     c                   EndIf

     c                   EndSR                                                  $Routing
