      ** (C) Copyright Progressive Data Solutions, Inc., 1999
     h/title FIXMIDEND: Changes midterm option 161 description
      ****************************************************************
      ** Program Description:                                       **
      **   This program changes midterm option 161 description.     **
      **                                                            **
      ** Called from: Command line                                  **
      **                                                            **
      ** Calls to:    None                                          **
      **                                                            **
      ** Programmer: C:113                                          **
      ** Created:    06/29/99  PROJECT: CM027                       **
      ****************************************************************
     FWTMYP     uf   e             disk                                         Midterm Endorsement

     D    xxDesc       s                   inz('OWNER/OFFICER(NOMINAL) CHANGES')
     D                                     like(MYDESC)

     C                   read      pmy01                                  60

     C                   DOW       *in60 = *off

     C                   IF        MYSORT = 161
     C                   eval      MYDESC = *blanks
     C                   eval      MYDSC1 = *blanks
     C                   eval      MYDESC = xxDesc
     C                   eval      MYDSC1 = xxDesc
     C                   update    pmy01
     C                   ENDIF

     C                   read      pmy01                                  60

     C                   ENDDO

     C                   return
