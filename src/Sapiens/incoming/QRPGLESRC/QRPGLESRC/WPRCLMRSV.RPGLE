     P*====================================================================
     P* GetInitRsvFlag - Determine the value of the initial reserve flag
     P*
     P*  GetInitRsvFlag - Returns 'Y' or blank
     P*   co#       - accepts the company #                (required)
     P*   group     - accepts the group                    (required)
     P*   fyr       - accepts the fyr                      (required)
     P*   claim     - accepts the claim                    (required)
     P*====================================================================
     DGetInitRsvFlag   pr                  like(RHinitRsv)
     D co#                                 like(RHco#)  const
     D group                               like(RHfnd)  const
     D fyr                                 like(RHfyr)  const
     D claim                               like(RHcase) const
