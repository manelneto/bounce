SET search_path TO lbaw23114;

-- Sample data for the 'users' table
INSERT INTO users (username, email, password, register_date, administrator, blocked, image)
VALUES
    ('user1', 'user1@example.com', 'password1', '2023-10-16', false, false, 'image1.jpg'),
    ('user2', 'user2@example.com', 'password2', '2023-10-16', false, false, 'image2.jpg'),
    ('user3', 'user3@example.com', 'password3', '2023-10-16', false, true, 'image3.jpg'),
    ('admin', 'admin@example.com', 'adminpass', '2023-10-16', true, false, 'admin.jpg');

-- Sample data for the 'notification' table
INSERT INTO notification (content, date, read, id_user)
VALUES
    ('You have a new message.', '2023-10-16', false, 1),
    ('Reminder: Upcoming event tomorrow.', '2023-10-16', false, 2),
    ('Account suspension notice.', '2023-10-16', false, 3),
    ('Welcome to the community!', '2023-10-16', false, 4);

-- Sample data for the 'badge' table
INSERT INTO badge (name)
VALUES
    ('Beginner'),
    ('Intermediate'),
    ('Expert');

-- Sample data for the 'user_earns_badge' table
INSERT INTO user_earns_badge (id_user, id_badge)
VALUES
    (1, 1),
    (2, 2),
    (3, 1),
    (4, 3);

-- Sample data for the 'tag' table
INSERT INTO tag (name)
VALUES
    ('Java'),
    ('Python'),
    ('SQL');

-- Sample data for the 'user_follows_tag' table
INSERT INTO user_follows_tag (id_user, id_tag)
VALUES
    (1, 1),
    (2, 2),
    (3, 3),
    (4, 1);
-- Continue with similar INSERT statements for other tables

-- Sample data for the 'community' table
INSERT INTO community (name)
VALUES
    ('Programming Enthusiasts'),
    ('Tech Support'),
    ('Cooking Lovers'),
    ('Travel Enthusiasts');

-- Sample data for the 'user_follows_community' table
INSERT INTO user_follows_community (id_user, id_community)
VALUES
    (1, 1),
    (2, 2),
    (3, 3),
    (4, 4);
-- Sample data for the 'user_moderates_community' table
INSERT INTO user_moderates_community (id_user, id_community)
VALUES
    (1, 1),
    (2, 2),
    (3, 3),
    (4, 4);

-- Sample data for the 'reputation' table
INSERT INTO reputation (id_user, id_community, rating, expert)
VALUES
    (1, 1, 100, false),
    (2, 2, 50, false),
    (3, 3, 75, false),
    (4, 4, 150, true);

-- Sample data for the 'question' table
INSERT INTO question (content, date, file, last_edited, title, id_user, id_community)
VALUES
    ('How to write a Python function?', '2023-10-16', 'python_function.pdf', NULL, 'Python Help', 1, 1),
    ('SQL JOIN example needed.', '2023-10-16', NULL, '2023-10-16', 'SQL Join Question', 2, 2),
    ('Best pasta recipe?', '2023-10-16', 'pasta_recipe.jpg', '2023-10-16', 'Pasta Lovers', 3, 3),
    ('Travel recommendations for Europe?', '2023-10-16', NULL, NULL, 'Europe Travel', 4, 4);

-- Sample data for the 'user_follows_question' table
INSERT INTO user_follows_question (id_user, id_question)
VALUES
    (1, 1),
    (2, 2),
    (3, 3),
    (4, 4);

-- Sample data for the 'question_tags' table
INSERT INTO question_tags (id_question, id_tag)
VALUES
    (1, 2),
    (2, 3),
    (3, 1),
    (4, 3);

-- Sample data for the 'question_vote' table
INSERT INTO question_vote (id_question, id_user, likes)
VALUES
    (1, 2, true),
    (2, 1, true),
    (3, 4, false),
    (4, 3, true);

-- Sample data for the 'question_comment' table
INSERT INTO question_comment (content, date, last_edited, id_user, id_question)
VALUES
    ('Great question!', '2023-10-16', NULL, 2, 1),
    ('I can help with this.', '2023-10-16', NULL, 1, 2),
    ('Please share the recipe!', '2023-10-16', NULL, 4, 3),
    ('I have some travel tips.', '2023-10-16', NULL, 3, 4);

-- Sample data for the 'answer' table
INSERT INTO answer (content, date, file, last_edited, correct, id_user, id_question)
VALUES
    ('You can write a Python function by...', '2023-10-16', NULL, NULL, true, 2, 1),
    ('For SQL JOIN, you can use...', '2023-10-16', NULL, NULL, false, 3, 2),
    ('Here''s my favorite pasta recipe:', '2023-10-16', NULL, NULL, false, 4, 3),
    ('Europe has many amazing places...', '2023-10-16', NULL, '2023-10-16', false, 1, 4);

-- Sample data for the 'answer_vote' table
INSERT INTO answer_vote (id_answer, id_user, likes)
VALUES
    (1, 3, true),
    (2, 4, true),
    (3, 1, false),
    (4, 2, true);

-- Sample data for the 'answer_comment' table
INSERT INTO answer_comment (content, date, last_edited, id_user, id_answer)
VALUES
    ('This answer is helpful!', '2023-10-16', NULL, 2, 1),
    ('Can you explain it further?', '2023-10-16', NULL, 1, 2),
    ('I tried the recipe. It''s amazing!', '2023-10-16', NULL, 3, 3),
    ('Do you have more travel tips?', '2023-10-16', NULL, 4, 4);
