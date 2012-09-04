DROP VIEW recent_events_by_session_id;
DROP VIEW session_clients;
DROP TABLE sessions;
DROP TABLE clients;
DROP VIEW recent_events;
DROP TABLE events;

CREATE TABLE clients
  ( id            serial
  , name          text UNIQUE NOT NULL
  , last_sync     timestamp with time zone
  , PRIMARY KEY (id)
  );

CREATE TABLE sessions
  ( id            serial
  , client_id     int
  , PRIMARY KEY (id)
  , FOREIGN KEY (client_id) REFERENCES clients(id) ON DELETE CASCADE
  );

CREATE OR REPLACE VIEW session_clients AS
  SELECT clients.id AS client_id, sessions.id AS session_id, last_sync
    FROM clients, sessions
   WHERE clients.id = sessions.client_id;

CREATE TABLE events
  ( id        serial
  , time      timestamp with time zone
  , path      text
  , event     int
  , PRIMARY KEY (id)
  );

CREATE OR REPLACE VIEW recent_events AS
  SELECT DISTINCT ON (path) time, path, event
    FROM events
   ORDER BY path, time DESC;

CREATE OR REPLACE VIEW recent_events_by_session_id AS
  SELECT session_id, time, path, event
    FROM session_clients, recent_events
   WHERE (last_sync IS NULL) OR (time >= last_sync);
