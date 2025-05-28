#* @serializer unboxedJSON
#* @post /register
function(req,res) {
    return(userRegistrationWorkflow(req,res))
}

#* @serializer unboxedJSON
#* @post /login
function(req,res) {
    return(userLogin(req,res))
}

#* @param token:chr The JWT token received during initial authentication
#* @serializer unboxedJSON
#* @get /authenticate
function(req,res,token) {
    return(authenticateUser(req,res,token))
}

#* @param username:chr The username
#* @param new_password:chr The new password
#* @serializer unboxedJSON
#* @post /reset_password
function(req,res) {
    return(userResetPassword(req,res))
}

#* @param token:chr The verification token sent in the email
#* @serializer html
#* @get /verify_email
function(req,res,token) {
    return(verifyUserEmail(req,res,token))
}

