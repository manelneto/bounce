CREATE SCHEMA IF NOT EXISTS lbaw23114;
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

DROP FUNCTION IF EXISTS new_badge_notification CASCADE;
DROP FUNCTION IF EXISTS new_answer_comment_notification CASCADE;
DROP FUNCTION IF EXISTS new_question_comment_notification CASCADE;
DROP FUNCTION IF EXISTS new_vote_notification CASCADE;
DROP FUNCTION IF EXISTS new_answer_notification CASCADE;
DROP FUNCTION IF EXISTS check_answer_vote CASCADE;
DROP FUNCTION IF EXISTS check_question_vote CASCADE;
DROP FUNCTION IF EXISTS check_file_extension CASCADE;
DROP FUNCTION IF EXISTS check_expert_status CASCADE;
DROP FUNCTION IF EXISTS calculate_user_rating CASCADE;
DROP FUNCTION IF EXISTS update_content_on_user_deletion CASCADE;
DROP FUNCTION IF EXISTS prevent_self_vote_on_answer CASCADE;
DROP FUNCTION IF EXISTS prevent_self_vote_on_question CASCADE;


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
    date DATE NOT NULL DEFAULT CURRENT_DATE,
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


-- Índices

CREATE INDEX user_notification ON notification USING hash (id_user);


CREATE INDEX community_question ON question USING btree (id_community);
CLUSTER question USING community_question;


CREATE INDEX question_question_vote ON question_vote USING hash (id_question);


-- Adicionar à tabela question uma coluna para armazenar os ts_vectors computados
ALTER TABLE question
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE OR REPLACE FUNCTION question_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = (
            setweight(to_tsvector('english', NEW.title), 'A') ||
            setweight(to_tsvector('english', NEW.content), 'B')
        );
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF (NEW.title <> OLD.title OR NEW.content <> OLD.content) THEN
            NEW.tsvectors = (
                setweight(to_tsvector('english', NEW.title), 'A') ||
                setweight(to_tsvector('english', NEW.content), 'B')
            );
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER question_search_update
    BEFORE INSERT OR UPDATE ON question
    FOR EACH ROW
    EXECUTE PROCEDURE question_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX question_search_idx ON question USING GIN (tsvectors);


-- Adicionar à tabela answer uma coluna para armazenar os ts_vectors computados
ALTER TABLE answer
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE OR REPLACE FUNCTION answer_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = to_tsvector('english', NEW.content);
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF NEW.content <> OLD.content THEN
            NEW.tsvectors = to_tsvector('english', NEW.content);
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER answer_search_update
    BEFORE INSERT OR UPDATE ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE answer_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX answer_search_idx ON answer USING GIN (tsvectors);


-- Adicionar à tabela question_comment uma coluna para armazenar os ts_vectors computados
ALTER TABLE question_comment
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE OR REPLACE FUNCTION question_comment_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = to_tsvector('english', NEW.content);
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF NEW.content <> OLD.content THEN
            NEW.tsvectors = to_tsvector('english', NEW.content);
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER question_comment_search_update
    BEFORE INSERT OR UPDATE ON question_comment
    FOR EACH ROW
    EXECUTE PROCEDURE question_comment_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX question_comment_search_idx ON question_comment USING GIN (tsvectors);


-- Adicionar à tabela answer_comment uma coluna para armazenar os ts_vectors computados
ALTER TABLE answer_comment
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE OR REPLACE FUNCTION answer_comment_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = to_tsvector('english', NEW.content);
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF NEW.content <> OLD.content THEN
            NEW.tsvectors = to_tsvector('english', NEW.content);
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER answer_comment_search_update
    BEFORE INSERT OR UPDATE ON answer_comment
    FOR EACH ROW
    EXECUTE PROCEDURE answer_comment_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX answer_comment_search_idx ON answer_comment USING GIN (tsvectors);


-- Triggers


-- BR01
CREATE FUNCTION prevent_self_vote_on_question() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF NEW.id_user = (SELECT id_user FROM question WHERE id = NEW.id_question) THEN
        RAISE EXCEPTION 'Cannot vote on your own question';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER prevent_self_vote_on_question
    BEFORE INSERT OR UPDATE ON question_vote
    FOR EACH ROW
    EXECUTE PROCEDURE prevent_self_vote_on_question();


CREATE FUNCTION prevent_self_vote_on_answer() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF NEW.id_user = (SELECT id_user FROM answer WHERE id = NEW.id_answer) THEN
        RAISE EXCEPTION 'Cannot vote on your own answer';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER prevent_self_vote_on_answer
    BEFORE INSERT OR UPDATE ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE prevent_self_vote_on_answer();

-- BR02
CREATE FUNCTION update_content_on_user_deletion() RETURNS TRIGGER AS
$BODY$
BEGIN
    UPDATE users SET username = CONCAT('anonymous', OLD.id), email = CONCAT('anonymous', OLD.id) WHERE id = OLD.id;
    RETURN NULL;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER update_content_on_user_deletion
    BEFORE DELETE ON users
    FOR EACH ROW
    EXECUTE PROCEDURE update_content_on_user_deletion();


-- BR03
CREATE FUNCTION calculate_user_rating() RETURNS TRIGGER AS
$BODY$
DECLARE
    total_likes INTEGER;
    total_dislikes INTEGER;
    id_author INTEGER;
    id_c INTEGER;
BEGIN
    SELECT id_user INTO id_author
    FROM answer
    WHERE answer.id = NEW.id_answer;

    SELECT id_community INTO id_c
    FROM answer NATURAL JOIN question
    WHERE answer.id = NEW.id_answer;

    SELECT COUNT(*) INTO total_likes
    FROM question NATURAL JOIN answer JOIN answer_vote ON answer_vote.id_answer = answer.id 
    WHERE answer.id_user = id_author AND question.id_community = id_c AND answer_vote.likes = TRUE;

    SELECT COUNT(*) INTO total_dislikes
    FROM question NATURAL JOIN answer JOIN answer_vote ON answer_vote.id_answer = answer.id 
    WHERE answer.id_user = id_author AND question.id_community = id_c AND answer_vote.likes = FALSE;

    IF total_likes + total_dislikes = 0 THEN
        RETURN NEW;
    END IF;

    UPDATE reputation
    SET rating = 1000 * total_likes / (total_likes + total_dislikes)
    WHERE id_user = id_author AND id_community = id_c;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER calculate_user_rating
    AFTER INSERT OR UPDATE ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE calculate_user_rating();


-- BR04
CREATE FUNCTION check_expert_status() RETURNS TRIGGER AS
$BODY$
DECLARE
    total_badges INTEGER;
    user_badges INTEGER;
BEGIN
    SELECT COUNT(*) INTO total_badges FROM badge;
    SELECT COUNT(*) INTO user_badges FROM user_earns_badge WHERE id_user = NEW.id_user;

    IF total_badges = user_badges AND NEW.rating > 800 THEN
        SET NEW.expert = TRUE;
    ELSE
        SET NEW.expert = FALSE;
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_expert_status
    AFTER INSERT OR UPDATE ON reputation
    FOR EACH ROW
    EXECUTE PROCEDURE check_expert_status();


-- BR05
CREATE FUNCTION check_file_extension() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF NEW.file NOT LIKE '%.jpg' AND 
       NEW.file NOT LIKE '%.jpeg' AND 
       NEW.file NOT LIKE '%.png' AND 
       NEW.file NOT LIKE '%.txt' AND 
       NEW.file NOT LIKE '%.pdf' AND 
       NEW.file NOT LIKE '%.doc' THEN
       RAISE EXCEPTION 'Invalid file extension. Only jpg, jpeg, png, txt, pdf, doc are allowed.';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_file_extension_on_question
    BEFORE INSERT OR UPDATE ON question
    FOR EACH ROW
    EXECUTE PROCEDURE check_file_extension();

CREATE TRIGGER check_file_extension_on_answer
    BEFORE INSERT OR UPDATE ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE check_file_extension();


-- BR06
CREATE FUNCTION check_question_vote() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF EXISTS (SELECT id_user FROM question_vote WHERE id_question = NEW.id_question AND id_user = NEW.id_user) THEN
        RAISE EXCEPTION 'Each user can only vote once on each question.';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_question_vote
    BEFORE INSERT OR UPDATE ON question_vote
    FOR EACH ROW
    EXECUTE PROCEDURE check_question_vote();

CREATE FUNCTION check_answer_vote() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF EXISTS (SELECT id_user FROM answer_vote WHERE id_answer = NEW.id_answer AND id_user = NEW.id_user) THEN
        RAISE EXCEPTION 'Each user can only vote once on each answer.';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_answer_vote
    BEFORE INSERT ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE check_answer_vote();


-- Notificações

CREATE FUNCTION new_answer_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT id_user, title INTO author, q_title
    FROM question
    WHERE id = NEW.id_question;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new answer to your question: ', q_title, '!'), CURRENT_DATE, FALSE, author);
    
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_answer_notification
    AFTER INSERT ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE new_answer_notification();


CREATE FUNCTION new_vote_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT id_user, title INTO author, q_title
    FROM question
    WHERE id = NEW.id_question;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new vote on your question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_vote_notification
    AFTER INSERT ON question_vote
    FOR EACH ROW
    EXECUTE PROCEDURE new_vote_notification();


CREATE FUNCTION new_question_comment_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT id_user, title INTO author, q_title
    FROM question
    WHERE id = NEW.id_question;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new comment on your question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_question_comment_notification
    AFTER INSERT ON question_comment
    FOR EACH ROW
    EXECUTE PROCEDURE new_question_comment_notification();


CREATE FUNCTION new_answer_comment_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT answer.id_user, title INTO author, q_title
    FROM answer JOIN question ON answer.id_question = question.id
    WHERE answer.id = NEW.id_answer;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new comment on your answer to the question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_answer_comment_notification
    AFTER INSERT ON answer_comment
    FOR EACH ROW
    EXECUTE PROCEDURE new_answer_comment_notification();


CREATE FUNCTION new_badge_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    winner INTEGER;
    b_name VARCHAR(255);
BEGIN
    SELECT id_user, name INTO winner, b_name
    FROM user_earns_badge NATURAL JOIN badge
    WHERE id = NEW.id_badge;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new badge: ', b_name, '!'), CURRENT_DATE, FALSE, winner);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_badge_notification
    AFTER INSERT ON user_earns_badge
    FOR EACH ROW
    EXECUTE PROCEDURE new_badge_notification();


-- badges?

