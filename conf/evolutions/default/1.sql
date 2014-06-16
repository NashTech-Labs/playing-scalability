# --- First database schema

# --- !Ups

set ignorecase true;

CREATE TABLE book (
  id                        BIGINT NOT NULL AUTO_INCREMENT,
  name                      VARCHAR(255) NOT NULL,
  author                    VARCHAR(1000) NOT NULL,
  publish_date              TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  description               VARCHAR(255) NOT NULL,
  CONSTRAINT pk_book PRIMARY KEY (id))
;

# --- !Downs

drop table if exists book;
