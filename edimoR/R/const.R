CONST <- list(
    INVALID_TOKEN="
        <html>
        <head>
          <title>Invalid link</title>
        </head>
        <body>
          <h1>Invalid verification token!</h1>
          <p>Are you sure the link you received is correct?
          Please contact an administrator.</p>
        </body>
        </html>
        ",
    EXPIRED_TOKEN="
        <html>
        <head>
          <title>Expired link</title>
        </head>
        <body>
          <h1>Your verification link has expired!</h1>
          <p>Please request a new verification link (resend verification mail).</p>
        </body>
        </html>
        ",
    EMAIL_VERIFIED="
        <html>
        <head>
          <title>Verification success</title>
        </head>
        <body>
          <h1>Your email has been verified!</h1>
          <p>You can login to the app.</p>
        </body>
        </html>
        ",
    EMAIL_NOT_VERIFIED="
        <html>
        <head>
          <title>Verification failed</title>
        </head>
        <body>
          <h1>Your email has not been verified!</h1>
          <p>Please contact an administrator.</p>
        </body>
        </html>
        ",
    INVALID_USER="
        <html>
        <head>
          <title>Invalid user</title>
        </head>
        <body>
          <h1>User not found!</h1>
          <p>Please contact an administrator.</p>
        </body>
        </html>
        ",
    INTERNAL_ERROR="
        <html>
        <head>
          <title>Server error</title>
        </head>
        <body>
          <h1>Server internal error!</h1>
          <p>Please contact an administrator.</p>
        </body>
        </html>
        "
)

