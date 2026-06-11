      ** (C) Copyright Progressive Data Solutions, Inc., 2001
     h/title FIXF13RDT: Fix 3rd Assessment Transfer Code
      ****************************************************************
      ** Program Description:                                       **
      **   This program changes 3rd assesment transfer codes.       **
      **                                                            **
      ** Called from: Command line                                  **
      **                                                            **
      ** Calls to:    None                                          **
      **                                                            **
      ** Programmer: C: 90                                          **
      ** Created:    10/08/01  PROJECT: CM1024                      **
      ****************************************************************
     FWDF1P     uf   e             disk                                         Midterm Endorsement


     C                   read      pf101                                  60

     C                   DOW       *in60 = *off

     C                   IF        F11STT <> *blanks
     C                             or F12NDT <> *blanks
     C                   eval      F13RDT = '3T'
     C                   update    pf101
     C                   ENDIF

     C                   read      pf101                                  60

     C                   ENDDO

     C                   return
