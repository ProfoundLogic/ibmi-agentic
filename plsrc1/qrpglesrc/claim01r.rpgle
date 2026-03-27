
     H DFTACTGRP(*NO)

     FCLAIM01D  CF   E             WORKSTN
     F                                     SFile(CLAIMSFL : RRN)

     FCLAIMSP   UF A E           K DISK
     FCLAIMS2L  IF   E           K DISK    Rename(CLAIMS : CLAIMS2)

     D rrn             S             10I 0
     D program         S             10A
     D action          S             10A   Varying
     D done            S               N

     C* Keep running until F3=Exit is used
     C                   Dow       Not *In03
     C
     C* Load claims subfile
     C                   ExSr      LoadSubfile
     C
     C* Show main claims subfile screen
     C                   Write     BOTTOM
     C                   ExFmt     CLAIMCTL
     C                   Eval      Action = ''
     C
     C* Test if a subfile record was selected by user
     C                   ReadC     CLAIMSFL
     C                   If        Not %Eof()
     C                   Select
     C                   When      Option = '2'
     C                   Eval      Action = 'change'
     C                   ExSr      LoadDetails
     C                   ExSr      ShowDetails
     C                   If        Not *In03 and Not *In12
     C                   ExSr      Save
     C                   EndIf
     C                   When      Option = '4'
     C                   Eval      Action = 'delete'
     C                   ExSr      LoadDetails
     C                   ExSr      DoDelete
     C                   When      Option = '5'
     C                   Eval      Action = 'display'
     C                   ExSr      LoadDetails
     C                   ExSr      ShowDetails
     C                   EndSL
     C                   EndIf
     C
     C* Add Record Processing
     C                   If        *In06
     C                   Eval      Action = 'add'
     C                   ExSr      NewRecord
     C                   ExSr      ShowDetails
     C                   If        Not *In03 and Not *In12
     C                   ExSr      Save
     C                   EndIf
     C                   EndIf
     C
     C                   EndDo
     C
     C                   Eval       *InLr = *On                                 End Program


     C*=======================================================================
     C* Load Claims Subfile
     C*=======================================================================
     C     LoadSubfile   BegSr
     C
     C* Clear Subfile
     C                   Eval      RRN = 0
     C                   SetOn                                        70        Clear Subfile
     C                   Write     CLAIMCTL
     C                   SetOff                                       70
     C
     C* Read first record
     C                   If        C1DESC = ''
     C     C1NUMBER      SetLL     CLAIMS
     C                   Read(n)   CLAIMS
     C                   Else
     C     C1DESC        SetLL     CLAIMS2
     C                   Read      CLAIMS2
     C                   EndIf
     C
     C                   Dow       Not %Eof()
     C* Write to subfile
     C                   Add       1             RRN
     C                   Eval      Option = ''
     C                   Eval      S1NUMBER = CMNUMBER
     C                   Eval      CMMDY = %Dec(CMDATE : *MDY)
     C                   Write     CLAIMSFL
     C
     C* Read next record
     C                   If        C1DESC = ''
     C                   Read(n)   CLAIMS
     C                   Else
     C                   Read      CLAIMS2
     C                   EndIf
     C                   EndDo
     C
     C                   EndSr


     C*=======================================================================
     C* Load Claim Details
     C*=======================================================================
     C     LoadDetails   BegSr
     C
     C                   If        Action = 'change'
     C     S1NUMBER      Chain     Claims
     C                   SetOff                                       80        No Protect
     C                   Else
     C                   SetOn                                        80        Protect
     C     S1NUMBER      Chain(n)  Claims
     C                   EndIf
     C
     C                   If        %Found()
     C                   Eval      CMTOTEST = CMEQEST + CMOTHEST + CMPROPEST
     C                   Eval      CMTOTCOST = CMEQCOST + CMOTHCOST + CMPROPCOST
     C                   Else
     C                   Eval      CMTOTEST = 0
     C                   Eval      CMTOTCOST = 0
     C                   EndIf
     C
     C                   Eval      CMMDY = %Dec(CMDATE : *MDY)
     C
     C                   EndSr


     C*=======================================================================
     C* Show Claim Details
     C*=======================================================================
     C     ShowDetails   BegSr
     C
     C                   Eval      Message = Action
     C
     C                   Dou       done = *On
     C
     C
     C                   Eval      Program = ''
     C                   Eval      done = *on
     C                   ExFmt     DETAIL
     C
     C*                  If        *In06                                        Damages
     C*                  Eval      Program = 'CLAIM22R'
     C*                  EndIf
     C
     C*                  If        *In07                                        Work Orders
     C*                  Eval      Program = 'CLAIM23R'
     C*                  EndIf
     C
     C*                  If        *In08                                        Injured
     C*                  Eval      Program = 'CLAIM24R'
     C*                  EndIf
     C
     C*                  If        *In09                                        Police
     C*                  Eval      Program = 'CLAIM25R'
     C*                  EndIf
     C
     C*                  If        *In10                                        Towing
     C*                  Eval      Program = 'CLAIM26R'
     C*                  EndIf
     C
     C*                  If        *In11                                        Notes
     C*                  Eval      Program = 'CLAIM27R'
     C*                  EndIf
     C
     C*                  If        *In14                                        Photos
     C*                  Eval      Program = 'CLAIM28R'
     C*                  EndIf
     C
     C*                  If        *In15                                        Documents
     C*                  Eval      Program = 'CLAIM29R'
     C*                  EndIf
     C
     C                   If        Program <> ''
     C                   Eval      done = *off
     C                   Call      Program
     C                   Parm                    CMNUMBER
     C                   EndIf
     C
     C                   EndDo
     C
     C                   EndSr


     C*=======================================================================
     C* Save Changes
     C*=======================================================================
     C     Save          BegSr
     C
     C                   Eval      CMDATE = %Date(CMMDY : *MDY)
     C
     C                   If        Action = 'add'
     C                   Write     CLAIMS
     C                   EndIf
     C
     C                   If        Action = 'change'
     C                   Update    CLAIMS
     C                   EndIf
     C
     C                   EndSr


     C*=======================================================================
     C* Process Delete Option
     C*=======================================================================
     C     DoDelete      BegSr
     C
     C                   ExFmt     ConfirmDel
     C                   If        Not *In12
     C     CMNUMBER      Chain     CLAIMS
     C                   If        %Found()
     C                   Delete    CLAIMS
     C                   EndIf
     C                   EndIf
     C
     C                   EndSr


     C*=======================================================================
     C* Create New Record
     C*=======================================================================
     C     NewRecord     BegSr
     C
     C                   Eval      CMNUMBER = 0
     C     *HiVal        SetGT     CLAIMS
     C                   ReadP(n)  CLAIMS
     C                   Eval      S1NUMBER = CMNUMBER
     C                   Eval      CMNUMBER = S1NUMBER + 1
     C
     C                   SetOff                                       80        No Protect
     C                   Eval      CMDATE = %Date()
     C                   Eval      CMMDY = %Dec(CMDATE : *MDY)
     C                   Time                    CMTIME
     C                   Eval      CMDESC     = ''
     C                   Eval      CMUNITS    = 0
     C                   Eval      CMINJURED  = 0
     C                   Eval      CMHITRUN   = 'N'
     C                   Eval      CMCOMPLETE = 'N'
     C                   Eval      CMACTYPE   = ''
     C                   Eval      CMCOUNTY   = ''
     C                   Eval      CMADDR2    = ''
     C                   Eval      CMADDR1    = ''
     C                   Eval      CMCITY     = ''
     C                   Eval      CMSTATE    = ''
     C                   Eval      CMZIP      = 0
     C                   Eval      CMEQEST    = 0
     C                   Eval      CMOTHEST   = 0
     C                   Eval      CMPROPEST  = 0
     C                   Eval      CMEQCOST   = 0
     C                   Eval      CMOTHCOST  = 0
     C                   Eval      CMPROPCOST = 0
     C                   Eval      CMMOTOR    = 'N'
     C                   Eval      CMFLATTIRE = 'N'
     C                   Eval      CMBROKENW  = 'N'
     C                   Eval      CMVANDAL   = 'N'
     C                   Eval      CMPEDESTR  = 'N'
     C
     C                   EndSr

