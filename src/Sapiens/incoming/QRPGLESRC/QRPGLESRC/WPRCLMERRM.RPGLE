
     d* Proto-types

     d* Blanks Not Allowed
     d ChkNotBlank     pr              n
     d valueIn                       80
     d errorMsgOut                   50
     d errorMsgIdOut                 10

     d* Zeros Not Allowed
     d ChkNotZero      pr              n
     d valueIn                       15  2
     d errorMsgOut                   50
     d errorMsgIdOut                 10

     d* Blanks are Required.
     d ChkBlank        pr              n
     d valueIn                       80
     d errorMsgOut                   50
     d errorMsgIdOut                 10

     d* Zeros Are Required
     d ChkZero         pr              n
     d valueIn                       15  2
     d errorMsgOut                   50
     d errorMsgIdOut                 10

     d* Date must be valid
     d ChkDate         pr              n
     d valueIn                        8  0
     d errorMsgOut                   50
     d errorMsgIdOut                 10

     d* Dates must Ascend
     d ChkAscDate      pr              n
     d valueIn1                       8  0
     d valueIn2                       8  0
     d errorMsgOut                   50
     d errorMsgIdOut                 10

     d* Is date less than accident date
     d ChkLtAccDate    pr              n
     d valueIn                        8  0
     d accidentDate                   7  0
     d errorMsgOut                   50
     d errorMsgIdOut                 10

     d* Is date greater than accident date
     D ChkGtAccDate    Pr              n
     D valueIn                        8  0
     D compDtIn                       7  0
     D errorMsgOut                   50
     D errorMsgIdOut                 10

     d* Date must not be in future
     d ChkFutrDate     pr              n
     d valueIn                        8  0
     d errorMsgOut                   50
     d errorMsgIdOut                 10

     d* Invalid staff code
     d InvalidStaffCd  pr              n
     d staffCdIn                      3
     d errorMsgOut                   50
     d errorMsgIdOut                 10

     d* Bad char not Allowed
     d ChkBadChar      pr              n
     d valueIn                       80
     d errorMsgOut                   50
     d errorMsgIdOut                 10
