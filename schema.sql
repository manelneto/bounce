DROP SCHEMA IF EXISTS lbaw23114;
CREATE SCHEMA lbaw23114;
SET search_path TO lbaw23114;


DROP TABLE IF EXISTS answer_comment CASCADE;
DROP TABLE IF EXISTS answer_vote CASCADE;
DROP TABLE IF EXISTS answer CASCADE;
DROP TABLE IF EXISTS question_comment CASCADE;
DROP TABLE IF EXISTS question_vote CASCADE;
DROP TABLE IF EXISTS question_tags CASCADE;
DROP TABLE IF EXISTS user_follows_question CASCADE;
DROP TABLE IF EXISTS question CASCADE;
DROP TABLE IF EXISTS reputation CASCADE;
DROP TABLE IF EXISTS user_moderates_community CASCADE;
DROP TABLE IF EXISTS user_follows_community CASCADE;
DROP TABLE IF EXISTS community CASCADE;
DROP TABLE IF EXISTS user_follows_tag CASCADE;
DROP TABLE IF EXISTS tag CASCADE;
DROP TABLE IF EXISTS user_earns_badge CASCADE;
DROP TABLE IF EXISTS badge CASCADE;
DROP TABLE IF EXISTS notification CASCADE;
DROP TABLE IF EXISTS users CASCADE;


CREATE TABLE users ( -- a plural 'users' was adopted because 'user' is a reserved word in PostgreSQL
    id SERIAL PRIMARY KEY,
    username VARCHAR(255) NOT NULL CONSTRAINT user_username_uk UNIQUE,
    email VARCHAR(255) NOT NULL CONSTRAINT user_email_uk UNIQUE,
    password VARCHAR(255) NOT NULL,
    register_date DATE NOT NULL,
    administrator BOOLEAN NOT NULL DEFAULT FALSE,
    blocked BOOLEAN NOT NULL DEFAULT FALSE,
    image VARCHAR(255),
    CONSTRAINT users_register_date_ck CHECK (register_date <= CURRENT_DATE)
);

CREATE TABLE notification (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL,
    read BOOLEAN NOT NULL DEFAULT FALSE,
    id_user INTEGER NOT NULL REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT notification_date_ck CHECK (date <= CURRENT_DATE)
);

CREATE TABLE badge (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL CONSTRAINT badge_name_uk UNIQUE
);

CREATE TABLE user_earns_badge (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_badge INTEGER REFERENCES badge (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_user, id_badge)
);

CREATE TABLE tag (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL CONSTRAINT tag_name_uk UNIQUE
);

CREATE TABLE user_follows_tag (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_tag INTEGER REFERENCES tag (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_user, id_tag)
);

CREATE TABLE community (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL CONSTRAINT community_name_uk UNIQUE
);

CREATE TABLE user_follows_community (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_community INTEGER REFERENCES community (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_user, id_community)
);

CREATE TABLE user_moderates_community (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_community INTEGER REFERENCES community (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_user, id_community)
);

CREATE TABLE reputation (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_community INTEGER REFERENCES community (id) ON UPDATE CASCADE ON DELETE CASCADE,
    rating INTEGER NOT NULL DEFAULT 0,
    expert BOOLEAN NOT NULL DEFAULT FALSE,
    PRIMARY KEY (id_user, id_community),
    CONSTRAINT reputation_rating_ck CHECK (rating >= 0)
);

CREATE TABLE question (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL,
    file VARCHAR(255),
    last_edited DATE,
    title TEXT NOT NULL,
    id_user INTEGER NOT NULL REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_community INTEGER NOT NULL REFERENCES community (id) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT question_date_ck CHECK (date <= CURRENT_DATE),
    CONSTRAINT question_last_edited_ck CHECK (last_edited <= CURRENT_DATE AND last_edited >= date)
);

CREATE TABLE user_follows_question (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_question INTEGER REFERENCES question (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_user, id_question)
);

CREATE TABLE question_tags (
    id_question INTEGER REFERENCES question (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_tag INTEGER REFERENCES tag (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_question, id_tag)
);

CREATE TABLE question_vote (
    id_question INTEGER REFERENCES question (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    likes BOOLEAN NOT NULL, -- a plural 'likes' was adopted because 'like' is a reserved word in PostgreSQL
    PRIMARY KEY (id_question, id_user)
);

CREATE TABLE question_comment (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL,
    last_edited DATE,
    id_user INTEGER NOT NULL REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_question INTEGER NOT NULL REFERENCES question (id) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT question_comment_date_ck CHECK (date <= CURRENT_DATE),
    CONSTRAINT question_comment_last_edited_ck CHECK (last_edited <= CURRENT_DATE AND last_edited >= date)
);

CREATE TABLE answer (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL,
    file VARCHAR(255),
    last_edited DATE,
    correct BOOLEAN NOT NULL DEFAULT FALSE,
    id_user INTEGER NOT NULL REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_question INT NOT NULL REFERENCES question (id) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT answer_date_ck CHECK (date <= CURRENT_DATE),
    CONSTRAINT answer_last_edited_ck CHECK (last_edited <= CURRENT_DATE AND last_edited >= date)
);

CREATE TABLE answer_vote (
    id_answer INTEGER REFERENCES answer (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    likes BOOLEAN NOT NULL, -- a plural 'likes' was adopted because 'like' is a reserved word in PostgreSQL
    PRIMARY KEY (id_answer, id_user)
);

CREATE TABLE answer_comment (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL,
    last_edited DATE,
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_answer INTEGER REFERENCES answer (id) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT answer_comment_date_ck CHECK (date <= CURRENT_DATE),
    CONSTRAINT answer_comment_ck CHECK (last_edited <= CURRENT_DATE AND last_edited >= date)
);
