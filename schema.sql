
DROP TABLE IF EXISTS user CASCADE;
DROP TABLE IF EXISTS notification CASCADE;
DROP TABLE IF EXISTS badge CASCADE;
DROP TABLE IF EXISTS user_earns_badge CASCADE;
DROP TABLE IF EXISTS tag CASCADE;
DROP TABLE IF EXISTS user_follows_tag CASCADE;
DROP TABLE IF EXISTS community CASCADE;
DROP TABLE IF EXISTS user_follows_community CASCADE;
DROP TABLE IF EXISTS user_moderates_community CASCADE;
DROP TABLE IF EXISTS reputation CASCADE;
DROP TABLE IF EXISTS question CASCADE;
DROP TABLE IF EXISTS user_follows_question CASCADE;
DROP TABLE IF EXISTS question_tags CASCADE;
DROP TABLE IF EXISTS question_vote CASCADE;
DROP TABLE IF EXISTS question_comment CASCADE;
DROP TABLE IF EXISTS answer CASCADE;
DROP TABLE IF EXISTS answer_vote CASCADE;
DROP TABLE IF EXISTS answer_comment CASCADE;


-- Table: user
CREATE TABLE user (
    id INT PRIMARY KEY,
    username VARCHAR(255) NOT NULL UNIQUE,
    email VARCHAR(255) NOT NULL UNIQUE,
    password VARCHAR(255) NOT NULL,
    register_date DATE NOT NULL CHECK (register_date <= CURRENT_DATE),
    administrator BOOLEAN NOT NULL DEFAULT FALSE,
    blocked BOOLEAN NOT NULL DEFAULT FALSE,
    image BLOB
);

-- Table: notification
CREATE TABLE notification (
    id INT PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL CHECK (date <= CURRENT_DATE),
    read BOOLEAN NOT NULL DEFAULT FALSE,
    id_user INT NOT NULL,
    FOREIGN KEY (id_user) REFERENCES user(id)
);

-- Table: badge
CREATE TABLE badge (
    id INT PRIMARY KEY,
    name VARCHAR(255) NOT NULL UNIQUE
);

-- Table: user_earns_badge
CREATE TABLE user_earns_badge (
    id_user INT,
    id_badge INT,
    PRIMARY KEY (id_user, id_badge),
    FOREIGN KEY (id_user) REFERENCES user(id),
    FOREIGN KEY (id_badge) REFERENCES badge(id)
);

-- Table: tag
CREATE TABLE tag (
    id INT PRIMARY KEY,
    name VARCHAR(255) NOT NULL UNIQUE
);

-- Table: user_follows_tag
CREATE TABLE user_follows_tag (
    id_user INT,
    id_tag INT,
    PRIMARY KEY (id_user, id_tag),
    FOREIGN KEY (id_user) REFERENCES user(id),
    FOREIGN KEY (id_tag) REFERENCES tag(id)
);

-- Table: community
CREATE TABLE community (
    id INT PRIMARY KEY,
    name VARCHAR(255) NOT NULL UNIQUE
);

-- Table: user_follows_community
CREATE TABLE user_follows_community (
    id_user INT,
    id_community INT,
    PRIMARY KEY (id_user, id_community),
    FOREIGN KEY (id_user) REFERENCES user(id),
    FOREIGN KEY (id_community) REFERENCES community(id)
);

-- Table: user_moderates_community
CREATE TABLE user_moderates_community (
    id_user INT,
    id_community INT,
    PRIMARY KEY (id_user, id_community),
    FOREIGN KEY (id_user) REFERENCES user(id),
    FOREIGN KEY (id_community) REFERENCES community(id)
);

-- Table: reputation
CREATE TABLE reputation (
    id_user INT,
    id_community INT,
    rating INT NOT NULL DEFAULT 0 CHECK (rating >= 0),
    expert BOOLEAN NOT NULL DEFAULT FALSE,
    PRIMARY KEY (id_user, id_community),
    FOREIGN KEY (id_user) REFERENCES user(id),
    FOREIGN KEY (id_community) REFERENCES community(id)
);

-- Table: question
CREATE TABLE question (
    id INT PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL CHECK (date <= CURRENT_DATE),
    file BLOB,
    last_edited DATE CHECK (last_edited <= CURRENT_DATE AND last_edited >= date),
    title TEXT NOT NULL,
    id_user INT NOT NULL,
    id_community INT NOT NULL,
    FOREIGN KEY (id_user) REFERENCES user(id),
    FOREIGN KEY (id_community) REFERENCES community(id)
);

-- Table: user_follows_question
CREATE TABLE user_follows_question (
    id_user INT,
    id_question INT,
    PRIMARY KEY (id_user, id_question),
    FOREIGN KEY (id_user) REFERENCES user(id),
    FOREIGN KEY (id_question) REFERENCES question(id)
);

-- Table: question_tags
CREATE TABLE question_tags (
    id_question INT,
    id_tag INT,
    PRIMARY KEY (id_question, id_tag),
    FOREIGN KEY (id_question) REFERENCES question(id),
    FOREIGN KEY (id_tag) REFERENCES tag(id)
);

-- Table: question_vote
CREATE TABLE question_vote (
    id_question INT,
    id_user INT,
    like BOOLEAN NOT NULL,
    PRIMARY KEY (id_question, id_user),
    FOREIGN KEY (id_question) REFERENCES question(id),
    FOREIGN KEY (id_user) REFERENCES user(id)
);

-- Table: question_comment
CREATE TABLE question_comment (
    id INT PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL CHECK (date <= CURRENT_DATE),
    last_edited DATE CHECK (last_edited <= CURRENT_DATE AND last_edited >= date),
    id_user INT NOT NULL,
    id_question INT NOT NULL,
    FOREIGN KEY (id_user) REFERENCES user(id),
    FOREIGN KEY (id_question) REFERENCES question(id)
);

-- Table: answer
CREATE TABLE answer (
    id INT PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL CHECK (date <= CURRENT_DATE),
    file BLOB,
    last_edited DATE CHECK (last_edited <= CURRENT_DATE AND last_edited >= date),
    correct BOOLEAN NOT NULL DEFAULT FALSE,
    id_user INT NOT NULL,
    id_question INT NOT NULL,
    FOREIGN KEY (id_user) REFERENCES user(id),
    FOREIGN KEY (id_question) REFERENCES question(id)
);

-- Table: answer_vote
CREATE TABLE answer_vote (
    id_answer INT,
    id_user INT,
    like BOOLEAN NOT NULL,
    PRIMARY KEY (id_answer, id_user),
    FOREIGN KEY (id_answer) REFERENCES answer (id),
    FOREIGN KEY (id_user) REFERENCES user (id)
);

-- Table: answer_comment
CREATE TABLE answer_comment (
    id INT PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE CHECK (date <= CURRENT_DATE),
    last_edited DATE CHECK (last_edited <= CURRENT_DATE AND last_edited >= date),
    id_user INT,
    id_answer INT,
    FOREIGN KEY (id_user) REFERENCES user (id),
    FOREIGN KEY (id_answer) REFERENCES answer (id)
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
