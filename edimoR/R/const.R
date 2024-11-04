MDB_QUERIES <- list(
    USER_FROM_ID="SELECT `complete_name` FROM `users` WHERE `id`=?id",
    REGISTER_USER=paste0("INSERT INTO users (`username`,`complete_name`,",
        "`email`,`password`,`date_created`,`date_updated`,`last_login`,`role`)",
        " VALUES "),
)
