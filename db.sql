
CREATE ROLE dev;
GRANT CONNECT ON DATABASE livelines TO dev;

CREATE TABLE users
(
    id serial PRIMARY KEY,
    username TEXT UNIQUE NOT NULL,
    password TEXT NOT NULL,
    email TEXT UNIQUE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    last_login TIMESTAMP WITH TIME ZONE NULL DEFAULT NULL
);

CREATE INDEX users_username_idx ON users (username);
CREATE INDEX users_email_idx ON users (email);

GRANT UPDATE, INSERT, SELECT ON TABLE users TO dev;
GRANT ALL ON SEQUENCE users_id_seq TO dev;

CREATE TABLE lines
(
    id serial PRIMARY KEY,
    user_id INTEGER REFERENCES users (id) NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

GRANT UPDATE, INSERT, SELECT ON TABLE lines TO dev;
GRANT ALL ON SEQUENCE lines_id_seq TO dev;
