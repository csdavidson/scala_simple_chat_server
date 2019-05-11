package example

import java.net._
import java.io._

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Lock}
import scala.io._

object ChatServer extends App {
    implicit val exec_context: ExecutionContextExecutor = ExecutionContext.global

    case class UserConnection(
        username: String,
        socket: Socket,
        in_buf: Iterator[String],
        out_buf: PrintStream
    )

    val server  = new ServerSocket(9999)
    val users   = new mutable.HashMap[String, UserConnection]()
    val exitcmd = "!exit"

    /**
      * Set up a listening thread for new connections
      * @return Unit
      */
    def takeConnections(): Unit =
    {
        Future {
            while (true) {
                val conn: Socket            = server.accept()
                val out: PrintStream        = new PrintStream(conn.getOutputStream)
                val input: Iterator[String] = new BufferedSource(conn.getInputStream).getLines()

                registerUser(conn, out, input)
            }
        }
    }

    /**
      * Register a user in our map of users
      * @param conn     Socket
      * @param out_buf  PrintStream
      * @param in_buf   Iterator[String]
      * @return Unit
      */
    def registerUser(conn: Socket, out_buf: PrintStream, in_buf: Iterator[String]): Unit =
    {
        Future {
            out_buf.println("Welcome, please enter a user name: ")
            out_buf.flush()
            var username = in_buf.next()

            while (users.get(username).nonEmpty) {
                out_buf.println("That username is already taken, please try again: ")
                out_buf.flush()
                username = in_buf.next()
            }

            // Got a new username
            val user_conn = UserConnection(username, conn, in_buf, out_buf)
            users.put(username, user_conn)

            out_buf.println(s"Welcome, $username!")
            out_buf.flush()

            receiveChat(user_conn)
        }
    }

    /**
      * Set up a listening thread for this user's chat
      * @param user_conn UserConnection
      * @return Unit
      */
    def receiveChat(user_conn: UserConnection): Unit =
    {
        Future {
            var user_connected = true

            while (user_connected) {
                if (user_conn.in_buf.nonEmpty) {
                    // Get the next chat line from the user
                    val user_chat = user_conn.in_buf.next()

                    // Log to server's output
                    println(s"${user_conn.username}: $user_chat")

                    // Send messages to each other user
                    users.foreach{
                        case (_, conn) => if (user_conn != conn) {
                            conn.out_buf.println(s"${user_conn.username}: $user_chat")
                            conn.out_buf.flush()
                        }
                    }

                    // Handle quitting the chat
                    if (user_chat == exitcmd) {
                        handleExit(user_conn)
                        user_connected = false
                    }
                }
            }
        }
    }

    /**
      * Handle business logic for when users decide to quit
      * @param user_conn UserConnection
      * @return Unit
      */
    def handleExit(user_conn: UserConnection): Unit =
    {
        user_conn.socket.close()
        users.remove(user_conn.username)

        users.foreach{
            case (_, conn) => if (user_conn != conn) {
                conn.out_buf.println(s"${user_conn.username} has left the chat")
                conn.out_buf.flush()
            }
        }
    }

    // Run the listener for new connections
    takeConnections()

    // Keep the shop open
    while (true) {
        //
    }
}
