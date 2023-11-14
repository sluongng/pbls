fn main() -> pbls::Result<()> {
    let (connection, io_threads) = lsp_server::Connection::stdio();
    pbls::start(connection)?;
    io_threads.join()?;
    Ok(())
}
