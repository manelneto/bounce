SET search_path TO lbaw23114;

-- Sample data for the 'users' table
INSERT INTO users (id,username, email, password, register_date, administrator, blocked, image)
VALUES
    (1,'user1', 'user1@example.com', 'password1', '2023-10-16', false, false, 'image1.jpg'),
    (2,'user2', 'user2@example.com', 'password2', '2023-10-16', false, false, 'image2.jpg'),
    (3,'user3', 'user3@example.com', 'password3', '2023-10-16', false, true, 'image3.jpg'),
    (4,'admin', 'admin@example.com', 'adminpass', '2023-10-16', true, false, 'admin.jpg');

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


-- Pass the "prevent_self_vote_on_question" trigger
-- User 1 votes on a question not asked by themselves
INSERT INTO question_vote (id_question, id_user, likes) VALUES (2, 1, true);
/*
-- Fail the "prevent_self_vote_on_question" trigger
-- User 1 votes on their own question
INSERT INTO question_vote (id_question, id_user, likes) VALUES (1, 1, true);

-- Pass the "prevent_self_vote_on_answer" trigger
-- User 2 votes on an answer not provided by themselves
INSERT INTO answer_vote (id_answer, id_user, likes) VALUES (1, 2, true);

-- Fail the "prevent_self_vote_on_answer" trigger
-- User 2 votes on their own answer
INSERT INTO answer_vote (id_answer, id_user, likes) VALUES (2, 2, true);

-- Trigger BR02 - User Deletion (Update Content)
-- Example to delete a user (trigger to set username and email to 'anonymous')
DELETE FROM users WHERE id = 3;

-- Trigger BR03 - Calculate User Rating
-- User 2 (id_user=2) votes on an answer
-- User 2's rating should be recalculated in the 'reputation' table
INSERT INTO answer_vote (id_answer, id_user, likes) VALUES (1, 2, true);

-- Trigger BR04 - Check Expert Status
-- Update user 1's rating to make them an expert (rating > 800)
-- This should trigger the expert status check
UPDATE reputation SET rating = 1000 WHERE id_user = 1;

-- Trigger BR05 - Check File Extension on Question
-- Example of inserting a question with an allowed file extension
INSERT INTO question (content, date, file, title, id_user, id_community) VALUES ('Sample question', '2023-10-16', 'sample.jpg', 'Sample', 1, 1);

-- Example of inserting a question with a disallowed file extension
-- This should fail the trigger
-- INSERT INTO question (content, date, file, title, id_user, id_community) VALUES ('Invalid question', '2023-10-16', 'invalid.exe', 'Invalid', 1, 1);

-- Trigger BR06 - Check Question Vote
-- Example to pass the "check_question_vote" trigger
-- User 3 votes on a question that they haven't voted on before
INSERT INTO question_vote (id_question, id_user, likes) VALUES (1, 3, true);

-- Example to fail the "check_question_vote" trigger
-- User 1 votes on the same question again
-- This should raise an exception because each user can only vote once on each question
-- INSERT INTO question_vote (id_question, id_user, likes) VALUES (1, 1, true);

-- Example to fail the "check_answer_vote" trigger
-- User 2 votes on the same answer again
-- This should raise an exception because each user can only vote once on each answer
-- INSERT INTO answer_vote (id_answer, id_user, likes) VALUES (1, 2, true);

-- Pass the "new_answer_notification" trigger
-- User 3 receives a new answer to their question
INSERT INTO answer (content, date, correct, id_user, id_question) VALUES ('New answer', '2023-10-16', false, 2, 1);

-- Pass the "new_vote_notification" trigger
-- User 2 receives a new vote on their question
INSERT INTO question_vote (id_question, id_user, likes) VALUES (1, 2, true);

-- Pass the "new_question_comment_notification" trigger
-- User 4 receives a new comment on their question
INSERT INTO question_comment (content, date, id_user, id_question) VALUES ('New comment', '2023-10-16', 3, 3);

-- Pass the "new_answer_comment_notification" trigger
-- User 1 receives a new comment on their answer
INSERT INTO answer_comment (content, date, id_user, id_answer) VALUES ('New comment', '2023-10-16', 4, 1);

-- Pass the "new_badge_notification" trigger
-- User 1 earns a new badge
INSERT INTO user_earns_badge (id_user, id_badge) VALUES (1, 2);

-- Fail the "check_file_extension_on_question" trigger
-- Insert a question with an invalid file extension (should raise an exception)
-- INSERT INTO question (content, date, file, title, id_user, id_community) VALUES ('Invalid question', '2023-10-16', 'invalid.exe', 'Invalid', 1, 1);

-- Pass the "check_file_extension_on_answer" trigger
-- Insert an answer with a valid file extension
INSERT INTO answer (content, date, file, correct, id_user, id_question) VALUES ('Sample answer', '2023-10-16', 'sample.pdf', false, 2, 1);
*/
