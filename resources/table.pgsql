CREATE TABLE wschat_user (
  user_id varchar(255) NOT NULL,
  topic varchar(255) NOT NULL,
  UNIQUE(user_id, topic)
);