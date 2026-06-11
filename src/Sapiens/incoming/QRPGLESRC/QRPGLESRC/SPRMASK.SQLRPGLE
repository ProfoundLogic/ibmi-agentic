     D#ofMasks         c                   10
     ddsMaskingGlobal  ds                  qualified
     d flag1                          1    overlay(dsMaskingGlobal: 1)
     d flag2                          1    overlay(dsMaskingGlobal: 2)
     d flag3                          1    overlay(dsMaskingGlobal: 3)
     d flag4                          1    overlay(dsMaskingGlobal: 4)
     d flag5                          1    overlay(dsMaskingGlobal: 5)
     d user1                         10    overlay(dsMaskingGlobal: 6)
     d user2                         10    overlay(dsMaskingGlobal:16)
     d user3                         10    overlay(dsMaskingGlobal:26)
     d aryUser                       10    overlay(dsMaskingGlobal: 6) dim(3)
      *=====================================================================
      * Determine if masking is active
      *=====================================================================
     dIsMaskingActive  pr             1n
     d inMaskType                    25a   varying const

      *=====================================================================
      * Mask data - show only last 4
      *=====================================================================
     dMaskData         pr           100a   varying
     d inMaskType                    25a   varying const
     d inFieldName                   25a   varying const
     d inValue                      100a   varying const

      *=====================================================================
      * Unmask data - return value prior to masking
      *=====================================================================
     dUnMaskData       pr           100a   varying
     d inMaskType                    25a   varying const
     d inFieldName                   25a   varying const
     d inValue                      100a   varying const
