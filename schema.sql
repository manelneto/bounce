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


-- Triggers

-- BR01
CREATE TRIGGER prevent_self_vote_on_question
BEFORE INSERT ON question_vote
FOR EACH ROW
BEGIN
  IF NEW.id_user = (SELECT id_user FROM question WHERE id = NEW.id_question) THEN
    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Cannot vote on your own question';
  END IF;
END;

CREATE TRIGGER prevent_self_vote_on_answer
BEFORE INSERT ON answer_vote
FOR EACH ROW
BEGIN
  IF NEW.id_user = (SELECT id_user FROM answer WHERE id = NEW.id_answer) THEN
    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Cannot vote on your own answer';
  END IF;
END;

--BR02
CREATE TRIGGER prevent_self_answer
BEFORE INSERT ON answer
FOR EACH ROW
BEGIN
  IF NEW.id_user = (SELECT id_user FROM question WHERE id = NEW.id_question) THEN
    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Cannot answer your own question';
  END IF;
END;

-- BR03
CREATE TRIGGER prevent_self_comment_on_question
BEFORE INSERT ON question_comment
FOR EACH ROW
BEGIN
  IF NEW.id_user = (SELECT id_user FROM question WHERE id = NEW.id_question) THEN
    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Cannot comment on your own question';
  END IF;
END;

CREATE TRIGGER prevent_self_comment_on_answer
BEFORE INSERT ON answer_comment
FOR EACH ROW
BEGIN
  IF NEW.id_user = (SELECT id_user FROM answer WHERE id = NEW.id_answer) THEN
    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Cannot comment on your own answer';
  END IF;
END;

-- BR04
CREATE TRIGGER update_content_on_user_delete
BEFORE DELETE ON user
FOR EACH ROW
BEGIN
  DECLARE anonymous_user_id INT;
  SET anonymous_user_id = (SELECT id FROM user WHERE username = CONCAT('anonymous', OLD.id));

  UPDATE question SET id_user = anonymous_user_id WHERE id_user = OLD.id;
  UPDATE answer SET id_user = anonymous_user_id WHERE id_user = OLD.id;
  UPDATE question_comment SET id_user = anonymous_user_id WHERE id_user = OLD.id;
  UPDATE answer_comment SET id_user = anonymous_user_id WHERE id_user = OLD.id;
END;

-- BR05
CREATE PROCEDURE calculate_user_rating(IN user_id INT, IN community_id INT)
BEGIN
  DECLARE total_likes INT;
  DECLARE total_dislikes INT;
  DECLARE rating INT;

  SELECT COUNT(*) INTO total_likes FROM answer_vote
  JOIN answer ON answer_vote.id_answer = answer.id
  WHERE answer.id_user = user_id AND answer.id_community = community_id AND answer_vote.like = TRUE;

  SELECT COUNT(*) INTO total_dislikes FROM answer_vote
  JOIN answer ON answer_vote.id_answer = answer.id
  WHERE answer.id_user = user_id AND answer.id_community = community_id AND answer_vote.like = FALSE;

  SET rating = 1000 * total_likes / (total_likes + total_dislikes);

  UPDATE reputation SET rating = rating WHERE id_user = user_id AND id_community = community_id;
END;

-- BR06
CREATE TRIGGER check_expert_status
AFTER INSERT, UPDATE ON reputation
FOR EACH ROW
BEGIN
    DECLARE total_badges INT;
    DECLARE user_badges INT;

    SELECT COUNT(*) INTO total_badges FROM badge;
    SELECT COUNT(*) INTO user_badges FROM user_earns_badge WHERE id_user = NEW.id_user;

    IF total_badges = user_badges AND NEW.rating > 800 THEN
        UPDATE reputation SET expert = TRUE WHERE id_user = NEW.id_user AND id_community = NEW.id_community;
    ELSE
        UPDATE reputation SET expert = FALSE WHERE id_user = NEW.id_user AND id_community = NEW.id_community;
    END IF;
END;

-- BR07
CREATE TRIGGER check_file_extension_before_insert_or_update_on_answer
BEFORE INSERT, UPDATE ON answer
FOR EACH ROW
BEGIN
    IF NEW.file NOT LIKE 'jpg' AND 
       NEW.file NOT LIKE 'jpeg' AND 
       NEW.file NOT LIKE 'png' AND 
       NEW.file NOT LIKE 'txt' AND 
       NEW.file NOT LIKE 'pdf' AND 
       NEW.file NOT LIKE 'doc' THEN
        SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Invalid file extension. Only jpg, jpeg, png, txt, pdf, doc are allowed.';
    END IF;
END;

-- BR08
CREATE TRIGGER check_vote_before_insert_on_question_vote
BEFORE INSERT ON question_vote
FOR EACH ROW
BEGIN
    IF EXISTS (SELECT 1 FROM question_vote WHERE id_question = NEW.id_question AND id_user = NEW.id_user) THEN
        SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Each user can only vote once on each post.';
    END IF;
END;

CREATE TRIGGER check_vote_before_insert_on_answer_vote
BEFORE INSERT ON answer_vote
FOR EACH ROW
BEGIN
    IF EXISTS (SELECT 1 FROM answer_vote WHERE id_answer = NEW.id_answer AND id_user = NEW.id_user) THEN
        SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Each user can only vote once on each post.';
    END IF;
END;


-- Matilde quando leres isto não te esqueças dos novos triggers para as notificações
-- Um utilizador deve receber notificações de: respostas às próprias perguntas, votos nas próprias perguntas, comentários nas próprias perguntas/respostas e emblemas
-- (e pelas user stories é só, afinal não precisa de notificações das coisas que segue)
-- Deixo um exemplo de um trigger que usamos em LTW e pode ser útil por ser parecido (por causa da geração das strings das notificações)
-- O trigger era para quando um ticket atualizava o seu estado (aberto para fechado, por exemplo) e essa atualização tinha de ficar registada na tabela 'Change' tipo «Status: Open → Closed'
-- O operador || concatena strings!

-- DROP TRIGGER IF EXISTS TicketStatus;
-- CREATE TRIGGER TicketStatus
    -- AFTER UPDATE OF idStatus ON Ticket
    -- WHEN New.idStatus <> Old.idStatus OR (New.idStatus IS NOT NULL AND Old.idStatus IS NULL)
-- BEGIN
    -- INSERT INTO Change (date, description, idTicket) VALUES (date(), 'Status: ' || IFNULL((SELECT name FROM Status WHERE idStatus = Old.idStatus), 'None') || ' → ' || (SELECT name FROM Status WHERE idStatus = New.idStatus), New.idTicket);
-- END;

CREATE TRIGGER new_answer_notification AFTER INSERT ON Answer FOR EACH ROW
BEGIN
   IF NEW.author_id = (SELECT author_id FROM Question WHERE id = NEW.question_id) THEN
      INSERT INTO Notification(content, date, read, id_user) VALUES ('You received a new answer to your question!', CURRENT_DATE, FALSE, NEW.author_id);
   END IF;
END;


CREATE TRIGGER new_vote_notification AFTER INSERT ON Vote FOR EACH ROW
BEGIN
   IF NEW.user_id = (SELECT author_id FROM Question WHERE id = NEW.question_id) THEN
      INSERT INTO Notification(content, date, read, id_user) VALUES ('You received a new vote to your question!', CURRENT_DATE, FALSE, NEW.user_id);
   END IF;
END;


CREATE TRIGGER new_question_comment_notification AFTER INSERT ON Comment FOR EACH ROW
BEGIN
   IF NEW.user_id = (SELECT author_id FROM Question WHERE id = NEW.question_id) THEN
      INSERT INTO Notification(content, date, read, id_user) VALUES ('You received a new comment to your question!', CURRENT_DATE, FALSE, NEW.user_id);
   END IF;
END;

CREATE TRIGGER new_answer_comment_notification AFTER INSERT ON Comment FOR EACH ROW
BEGIN
   IF NEW.user_id = (SELECT author_id FROM Answer WHERE id = NEW.answer_id) THEN
      INSERT INTO Notification(content, date, read, id_user) VALUES ('You received a new comment to your answer!', CURRENT_DATE, FALSE, NEW.user_id);
   END IF;
END;

CREATE TRIGGER NewBadgeNotification AFTER INSERT ON UserBadge FOR EACH ROW
BEGIN
   INSERT INTO Notification(content, date, read, id_user) VALUES ('You received a new badge: ' || NEW.badge_name, CURRENT_DATE, FALSE, NEW.user_id);
END;


