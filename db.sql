CREATE TABLE users (
        id serial PRIMARY KEY,
        username TEXT UNIQUE NOT NULL, -- TODO: add index on username for login lookup
        password TEXT NOT NULL,
        email TEXT UNIQUE NOT NULL,
        created_at TIMESTAMP NOT NULL DEFAULT NOW(),
        last_login TIMESTAMP NULL DEFAULT NULL);

