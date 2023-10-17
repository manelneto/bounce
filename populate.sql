SET search_path TO lbaw23114;

-- Sample data for the 'users' table
-- Insert sample data into the 'users' table
INSERT INTO users (username, email, password, register_date, administrator, blocked, image)
VALUES
    ('user1', 'user1@example.com', 'password1', '2023-01-01', true, false, 'image1.jpg'),
    ('user2', 'user2@example.com', 'password2', '2023-02-01', false, false, 'image2.jpg'),
    ('user3', 'user3@example.com', 'password3', '2023-03-01', false, true, 'image3.jpg'),
    ('user4', 'user4@example.com', 'password4', '2023-04-01', false, false, 'image4.jpg');

-- Insert sample data into the 'notification' table
I

-- Insert sample data into the 'badge' table
INSERT INTO badge (name)
VALUES
    ('Badge 1'),
    ('Badge 2'),
    ('Badge 3'),
    ('Badge 4');

-- Insert sample data into the 'user_earns_badge' table
INSERT INTO user_earns_badge (id_user, id_badge)
VALUES
    (1, 1),
    (2, 2),
    (3, 3),
    (4, 4);

-- Insert sample data into the 'tag' table
INSERT INTO tag (name)
VALUES
    ('Tag 1'),
    ('Tag 2'),
    ('Tag 3'),
    ('Tag 4');

-- Insert sample data into the 'user_follows_tag' table
INSERT INTO user_follows_tag (id_user, id_tag)
VALUES
    (1, 1),
    (2, 2),
    (3, 3),
    (4, 4);

-- Insert sample data into the 'community' table
INSERT INTO community (name)
VALUES
    ('Community 1'),
    ('Community 2'),
    ('Community 3'),
    ('Community 4');

-- Insert sample data into the 'user_follows_community' table
INSERT INTO user_follows_community (id_user, id_community)
VALUES
    (1, 1),
    (2, 2),
    (3, 3),
    (4, 4);

-- Insert sample data into the 'user_moderates_community' table
INSERT INTO user_moderates_community (id_user, id_community)
VALUES
    (1, 1),
    (2, 2),
    (3, 3),
    (4, 4);

-- Insert sample data into the 'reputation' table
INSERT INTO reputation (id_user, id_community, rating, expert)
VALUES
    (1, 1, 100, true),
    (2, 2, 200, false),
    (3, 3, 300, true),
    (4, 4, 400, false);

-- Insert sample data into the 'question' table
INSERT INTO question (content, date, file, last_edited, title, id_user, id_community)
VALUES
    ('Question 1 content', '2023-01-05', 'file1.pdf', null, 'Question 1', 1, 1),
    ('Question 2 content', '2023-02-10', null, '2023-02-15', 'Question 2', 2, 2),
    ('Question 3 content', '2023-03-15', 'file3.txt', null, 'Question 3', 3, 3),
    ('Question 4 content', '2023-04-20', null, null, 'Question 4', 4, 4);

-- Insert sample data into the 'user_follows_question' table
INSERT INTO user_follows_question (id_user, id_question)
VALUES
    (1, 1),
    (2, 2),
    (3, 3),
    (4, 4);

-- Insert sample data into the 'question_tags' table
INSERT INTO question_tags (id_question, id_tag)
VALUES
    (1, 1),
    (2, 2),
    (3, 3),
    (4, 4);

-- Insert sample data into the 'question_vote' table
INSERT INTO question_vote (id_question, id_user, likes)
VALUES
    (1, 1, true),
    (2, 2, false),
    (3, 3, true),
    (4, 4, false);

-- Insert sample data into the 'question_comment' table
INSERT INTO question_comment (content, date, last_edited, id_user, id_question)
VALUES
    ('Comment 1', '2023-01-05', null, 1, 1),
    ('Comment 2', '2023-02-10', '2023-02-15', 2, 2),
    ('Comment 3', '2023-03-15', null, 3, 3),
    ('Comment 4', '2023-04-20', '2023-04-25', 4, 4);

-- Insert sample data into the 'answer' table
INSERT INTO answer (content, date, file, last_edited, correct, id_user, id_question)
VALUES
    ('Answer 1 content', '2023-01-05', 'file1.pdf', null, false, 1, 1),
    ('Answer 2 content', '2023-02-10', null, '2023-02-15', true, 2, 2),
    ('Answer 3 content', '2023-03-15', 'file3.txt', null, false, 3, 3),
    ('Answer 4 content', '2023-04-20', null, null, true, 4, 4);

-- Insert sample data into the 'answer_vote' table
INSERT INTO answer_vote (id_answer, id_user, likes)
VALUES
    (1, 1, true),
    (2, 2, false),
    (3, 3, true),
    (4, 4, false);

-- Insert sample data into the 'answer_comment' table
INSERT INTO answer_comment (content, date, last_edited, id_user, id_answer)
VALUES
    ('Answer Comment 1', '2023-01-05', null, 1, 1),
    ('Answer Comment 2', '2023-02-10', '2023-02-15', 2, 2),
       ('Answer Comment 3', '2023-03-15', null, 3, 3),
    ('Answer Comment 4', '2023-04-20', '2023-04-25', 4, 4);

-- You can continue with INSERT statements for other tables if needed.

-- Make sure to generate appropriate sample data for each row in the remaining tables.

SELECT * FROM notification;





