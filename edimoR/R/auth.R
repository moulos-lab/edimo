#* @param username:chr The username
#* @param name:chr The complete name
#* @param email:chr The user email
#* @param password:chr The password
#* @param role:chr The user role
#* @serializer unboxedJSON
#* @post /register
function(req,res,username,name,email,password,role) {
    return(userRegistrationWorkflow(req,res,username,name,email,password,role))
}

#* @param username:chr The username
#* @param password:chr The password
#* @serializer unboxedJSON
#* @post /login
function(req,res,username,password) {
    return(userLogin(req,res,username,password))
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
function(req,res,username,new_password) {
    return(userResetPassword(req,res,username,new_password))
}

#* @param token:chr The verification token sent in the email
#* @serializer unboxedJSON
#* @get /verify_email
function(req,res,token) {
    return(verifyUserEmail(req,res,token))
}
