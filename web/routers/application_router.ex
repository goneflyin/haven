defmodule ApplicationRouter do
  use Dynamo.Router

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
    conn.resp(200, JSON.generate [message: "welcome to jsonified haven!"])
  end
end
