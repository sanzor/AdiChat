

CREATE TABLE wsuser (
    id SERIAL PRIMARY KEY,
    email VARCHAR(255) NOT NULL UNIQUE,
    password VARCHAR(255) NOT NULL,
    name VARCHAR(255) NOT NULL
);

-- Topic table
CREATE TABLE topic (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) UNIQUE,
    created_by VARCHAR(255),
    created_at TIMESTAMPTZ
);



-- Create an index on the name column for faster searches
CREATE INDEX idx_topics_name ON topic (name);


-- User-Topic table
CREATE TABLE user_topic (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES wsuser(id),
    topic_id INTEGER REFERENCES topic(id),
    created_at TIMESTAMP NOT NULL DEFAULT NOW()
);

-- Message table
CREATE TABLE message (
    id SERIAL PRIMARY KEY,
    topic_id INTEGER REFERENCES topic(id),
    user_id INTEGER REFERENCES wsuser(id),
    message VARCHAR(255),
    created_at TIMESTAMPTZ,
    timezone VARCHAR(255)
);