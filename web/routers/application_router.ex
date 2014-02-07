defmodule ApplicationRouter do
  use Dynamo.Router
  filter JSON.Dynamo.Filter

  forward "/register", to: RegistrarRouter

  prepare do
    conn.put_resp_header "Content-Type", "application/json"
  end

  # It is common to break your Dynamo into many
  # routers, forwarding the requests between them:
  # forward "/posts", to: PostsRouter

  get "/" do
    conn = conn.assign(:title, "Welcome to Dynamo!")
    render conn, "index.html"
  end

  get "/somejson" do
    conn.put_private :result_object, [message: "welcome to jsonified haven with a FILTER!"]
  end
end
