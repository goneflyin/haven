defmodule ApplicationRouter do
  use Dynamo.Router
  filter JSON.Dynamo.Filter

  forward "/services", to: RegistrarRouter

  get "/" do
    conn = conn.assign(:title, "Welcome to Haven!")
    render conn, "index.html"
  end

  get "/somejson" do
    conn.put_private :result_object, [message: "welcome to jsonified haven with a FILTER!"]
  end
end
